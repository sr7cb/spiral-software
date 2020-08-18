Class(NewScalarProduct, BaseOperation, rec(
  __call__ := (self, rv) >> SPL(WithBases(self, rec(
    rv := rv,
    _children := [rv]
  ))),
  dims := self >> [self.rv.element.domain(), V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.rv, ")")
));

Class(DenseScalarProduct, BaseOperation, rec(
  __call__ := (self, rv) >> SPL(WithBases(self, rec(
    rv := rv,
    _children := [rv]
  ))),
  dims := self >> [self.rv.element.domain(), V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.rv, ")")
));

Class(SparseScalarProduct, BaseOperation, rec(
  __call__ := (self, rv) >> SPL(WithBases(self, rec(
    rv := rv,
    _children := [rv]
  ))),
  dims := self >> [self.rv.element.domain(), V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.rv, ")")
));



Class(sparse_nth3, Loc, rec(
    __call__ := (self, list, idx) >> WithBases(self,
        rec(operations := NthOps,
			list := list,
            idx := toExpArg(idx))).setType().cfold(),

    can_fold := self >> self.idx _is funcExp or (IsValue(self.idx) and
                  (IsValue(self.list) or (IsVar(self.list) and IsBound(self.list.var)) or self.list _is apack)),
    cfold := self >> When(self.can_fold(), self.eval(), self),
 
    rChildren := self >> [self.list, self.idx],
    rSetChild := rSetChildFields("list", "idx"),
	
	eval := meth(self)
		local result, iterator;
		result := 0;
		iterator := 1;
		while iterator <= Length(self.list.var) and result = 0 do
			if self.idx = self.list.var[iterator].pair.first then
				#Print("if\n");
				result := self.list.var[iterator].pair.second;
				iterator := iterator + 1;
				break();
				#Print(iterator);
			else
				#Print("else\n");
				result := V(0);
				iterator := iterator + 1;
				#Print(iterator);
			fi;
		od;
		return result;
		end,
		
	#	result := var.fresh_t("result", TInt);
	#	g := var.fresh_t("g", TInt);
	#	decl[result, g], chain(
	#	loopf(g, V(1), Length(fds.var),
	#	chain(IF(eq(x, fds.var[g].pair.first), 
	#	assign(result,fds.var[g].pair.second), 
	#	assign(result,V(0))))), 
	#	return result);
	#  end;

    computeType := self >> Cond(
	IsArrayT(self.list.range()), self.list.range(),
        ObjId(self.list.t) = TSym, TSym("Containee"), #used with C++ container objects (EnvList)
        self.list.t = TUnknown,  self.list.range(),
	Error("Unknown types of 1st argument <self.list> in ", ObjId(self))
    ),
));


Class(TStruct, CompositeTyp, rec(
	__call__ := arg >> let(
	self := arg[1],
	fields := CopyFields(arg[2]),
	WithBases(self,
	rec(fields := fields, operations := TypOps))),
    print := self >> Print(self.name,"(", self.fields.name, ", ", self.fields.key, ", ", self.fields.val, ")"),
));

Class(TPair, TStruct, rec(
   __call__ := arg >> let(
   self := arg[1],
   pair := CopyFields(arg[2]),
   WithBases(self, 
   rec(pair := pair, operations := TypOps))),
  
   print := self >> Print(self.name,"(",self.pair.first,", ", self.pair.second,")"),
));


Class(KVPair, CompositeTyp, rec(
	__call__ := (self, first ,second) >> WithBases(self,rec( 
		first := first,
		second := second,
		operations := TypOps)),
		#t := Checked(IsType(t), t),	
		first := self >> self.first,
		second := self >> self.second,
	print := self >> Print(self.name,"(",self.first,", ",self.second,")"),
));


SparseMatSPL := function ( S )
  local M, t, v, i;
  t := 1;
  v := 1;
  i := 1;
  M := NullMat(1, S.size2);
  while t < S.size2 do 
	if t = S.list2[1][i] then
		M[1][t] := S.list1[1][v];
		t := t + 1;
		v := v + 1;
		i := i + 1;
	else
		M[1][t] := 0;
		t := t + 1;
	fi;
  od;
  return M;
  end;

#F sparse_nth(<loc>, <idx>) -- symbolic representation of array access
#F
Class(sparse_nth, Loc, rec(
    __call__ := (self, loc, idx) >> WithBases(self,
        rec(operations := NthOps,
            loc := toExpArg(loc),
            idx := toExpArg(idx))).setType().cfold(),

    can_fold := self >> self.idx _is funcExp or (IsValue(self.idx) and
                  (IsValue(self.loc) or (IsVar(self.loc) and IsBound(self.loc.value)) or self.loc _is apack)),
    cfold := self >> When(self.can_fold(), self.eval(), self),

    rChildren := self >> [self.loc, self.idx],
    rSetChild := rSetChildFields("loc", "idx"),

    ev := self >> let(e := self.eval(),
	Cond(IsBound(e.v), e.v, e)),

    eval := self >> let(loc := self.loc.eval(), idx := self.idx.eval(),
        Cond(IsList(loc),
				 Cond(idx.v >= Length(loc), errExp(self.t), Cond(idx.v = loc[1][x][1], V(0), V(loc))),
			 idx _is funcExp,
                 self.t.value(idx.args[1]),
             not IsValue(idx),
                 self,
             idx.v < 0,
                 errExp(self.t),
             loc _is apack,
                 Cond(idx.v >= Length(loc.args), errExp(self.t), loc.args[idx.v+1]),
             IsValue(loc),
                 Cond(idx.v >= Length(loc.v), errExp(self.t), V(loc.v[idx.v+1])),
             IsVar(loc) and IsBound(loc.value),
                 Cond(idx.v >= Length(loc.value.v), errExp(self.t), V(loc.value.v[idx.v+1])),
             self)),

    computeType := self >> Cond(
	IsPtrT(self.loc.t) or IsArrayT(self.loc.t) or IsListT(self.loc.t), self.loc.t.t,
        ObjId(self.loc.t)=TSym, TSym("Containee"), #used with C++ container objects (EnvList)
        self.loc.t = TUnknown,  self.loc.t,
	Error("Unknown types of 1st argument <self.loc> in ", ObjId(self))
    ),

    isExpComposite := true
));




#F FDataOfs(<datavar>, <len>, <ofs>)
#
Class(FDataSparseOfs, Function, rec(
    #__call__ := (self, datavar, len, ofs) >> WithBases(self, rec(
    #var := datavar,
    #operations := PrintOps,
    #ofs := toExpArg(ofs),
    #len := Checked(IsPosIntSym(len), len)
    #)),
   __call__ := arg >> let(
       self := arg[1],
       object := arg[2],
	   len := arg[3],
       ofs := arg[4],
	   WithBases(self, rec(object := object, len := len, ofs := ofs, operations := PrintOps))),

   print := self >> Print(self.name, "(", self.object, ", ", self.len, ", ", self.ofs, ")"),
   rChildren := self >> [ self.object, self.len, self.ofs ],
   rSetChild := rSetChildFields("object", "len", "ofs"),

# <-Daniele's changes
#    rChildren := self >> [ self.var, self.len, self.ofs],
#    rSetChild := rSetChildFields("var", "len", "ofs"),

#    rChildren := self >> [ self.var, self.len, self.ofs, self._domain, self._range],
#    rSetChild := rSetChildFields("var", "len", "ofs", "_domain", "_range"),
#    from_rChildren := (self, rch) >> ObjId(self)(rch[1], rch[2], rch[3]).setDomain(rch[4]).setRange(rch[5]),
# ->

    domain := self >> self.len,
    print := self >> Print(self.name,"(",self.var,", ",self.len,", ",self.ofs,")"),

    at := (self, n) >> When(IsInt(n) and IsValue(self.ofs) and IsBound(self.var.value),
        self.var.value.v[n + self.ofs.v + 1],
        nth(self.var, n + self.ofs)),

    tolist := self >> List([0..EvalScalar(self.len-1)], i -> nth(self.var, self.ofs+i)),
    lambda := self >> let(x := Ind(self.domain()), Lambda(x, nth(self.var, self.ofs+x))),

	range := self >> self.object.t,
    domain := self >> self.len,
    #range := self >> When(self._range=false, self.var.t.t, self._range),
    inline := true,
    free := self >> self.ofs.free()
));

#F FData(<datavar>) -- symbolic function i -> datavar[i],
#F
#F domain = datavar.range
#F range = datavar.t
#F
#F
Class(FDataSparse, Function, rec(
   __call__ := arg >> let(
       self := arg[1],
       object := arg[2],
       var := Cond(IsList(arg[3]), arg[3], [arg[3]]),
	   WithBases(self, rec(var := var, object := object, operations := PrintOps))),

   print := self >> Print(self.name, "(", self.object, ", ", self.var, ")"),
   rChildren := self >> [ self.var ],
   rSetChild := rSetChildFields("var"),

   at := (self, n) >> self.lambda().at(n),
   tolist := self >> self.lambda().tolist(),
   lambda := self >> let(x := Ind(self.domain()), Lambda(x, sparse_nth3(self, x))),
   
   domain := self >> self.object.size,
   range := self >> self.object.t,

   inline := true,
   free := self >> Set([]),
   part := (self, len, ofs) >> FDataSparseOfs(self.var, len, ofs),
));

Declare(TSparse);



Class(sparse_nth2, BaseOperation, rec(
	__call__ := (self, sa, index) >> SPL(WithBases(self, rec(
	  sa := sa,
	  index := index,
	  _children := [index]
	  ))),
	  dims := self >> [self.index, V(1)],
	  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
	  doNotMarkBB := true,
));

Class(TSparse2, SumsBase, BaseMat, rec(
    _short_print := true,
	rChildren := self >> [self.t, self.size1, self.list1, self.size2, self.list2],
	rSetChild := rSetChildFields("t","size1", "list1", "size2", "list2"),
	dims := self >> [self.size1, self.size2],

	new := (self, t, size1, list1, size2, list2) >> SPL(WithBases(self,
		rec(dimensions := [size1, size2],
		size1 := size1,
		size2 := size2,
		t := t,
		list1 := list1,
		list2 := list2))),
	
));

Class(struct_nth, nth, rec(
	__call__ := (self, loc, elem, idx) >> WithBases(self, rec(
		loc := loc,
		elem := elem,
		idx := idx,
		operations := NthOps
	)),
	rChildren := self >> [self.loc, self.elem, self.idx],
	rSetChild := rSetChildFields("loc", "elem", "idx"),
	#print := (self, i, si) >> Print(self.name, "
));

Class(TSparse, TArrayBase, rec(
	__call__ := (self, t, ring, size) >> 
			WithBases(self, rec(
			t    := Checked(IsType(t), t),
			ring := ring,
			size := Checked(IsPosInt0Sym(size), size),
			operations := TypOps)),
	isSparseT := true,
    vtype := (self, v) >> TSparse(self.t.vtype(v), self.size/v),
    toPtrType := self >> TPtr(self.t),
    doHashValues := true,
    dims := self >> Cond(
        ObjId(self.t)=TSparse, [self.size] :: self.t.dims(),
        [self.size]),
#	print := self >> Print(self.__name__, "(", self.t, ", ", self.size1, ", ", self.list1, ", ", self.size2, ", ", self.list2, ", ", self.sa, ")"),	
	print := self >> Print(self.__name__, "(", self.t, ", ", self.ring, ", ", self.size, ")"),

	get_iterator := self >> let( 
		itr := var.fresh_t("itr",TInt),
		decl([itr], chain(
		assign(itr, V(0))))),
	
	num_nonzeros := (self, x) >> let( 
		itr := var.fresh_t("itr", TInt),
		result := var.fresh_t("result", TInt),
		decl([itr, result], chain(
		assign(result, V(0)),
		loopw(neq(struct_nth(x,"value", itr), "NULL"), chain(
		if1(neq(struct_nth(x,"value", itr), V(0)), chain(
			assign(result, add(result, V(1)))))))))),
));


Class(if4, Command, rec(
  __call__ := (self, if_cond, if_cmd, else_cmd) >> WithBases(self, rec(
    if_cond := toExpArg(if_cond),
    if_cmd := Checked(IsCommand(if_cmd), if_cmd),
    else_cmd := Checked(IsCommand(else_cmd), else_cmd),
    operations := CmdOps
  )),
  rChildren := self >> [self.if_cond, self.if_cmd, self.else_cmd],
  rSetChild := rSetChildFields("if_cond", "if_cmd", "else_cmd"),
  print := (self, i, si) >> Print(self.__name__, "(", self.if_cond, ",\n",
        Blanks(i+si), self.if_cmd.print(i+si, si), ",\n",
        Blanks(i+si), self.else_cmd.print(i+si, si), "\n",
        Blanks(i), ")"
  )
));

CUnparser.TSparse := (self,o,i,is) >> Print(Blanks(i),
	"struct sparse_array", o.name);

CUnparser.struct_nth := (self,o,i,is) >> Print(Blanks(i),
	self(o.loc,i,is), ".", o.elem, "[", o.idx, "]");

CUnparser.if4 := (self,o,i,is) >> Print(Blanks(i),
    "if (", self(o.if_cond,i,is), ") {\n", self(o.if_cmd,i+is,is), Blanks(i), "}",
    " else {\n", self(o.else_cmd,i+is,is), Blanks(i), "}\n");

#Class(sparse_nth, Command, rec(
#  __call__ := (self, loc, idx) >> WithBases(self, rec(
#	loc := loc,
#    idx := idx,
#	operations := CmdOps
#  )),
#  rChildren := self >> [self.if_cond, self.if_cmd, self.else_cmd],
#  rSetChild := rSetChildFields("if_cond", "if_cmd", "else_cmd"),
#  print := (self, i, si) >> Print(self.__name__, "(", self.if_cond, ",\n",
#        Blanks(i+si), self.if_cmd.print(i+si, si), ",\n",
#        Blanks(i+si), self.else_cmd.print(i+si, si), "\n",
#        Blanks(i), ")"
#  )
#));
#
#CUnparser.sparse_nth := (self,o,i,is) >> Print(Blanks(i),
#    "if (", self(o.if_cond,i,is), ") {\n", self(o.if_cmd,i+is,is), Blanks(i), "}",
#    " else {\n", self(o.else_cmd,i+is,is), Blanks(i), "}\n");

DefaultSumsGen.DenseScalarProduct := (self, o, opts) >> o;


DefaultSumsGen.SparseScalarProduct := (self, o, opts) >> o;

DefaultSumsGen.NewScalarProduct := (self, o, opts) >> o;

DefaultSumsGen.TSparse := (self, o, opts) >> o;

DefaultSumsGen.sparse_nth2 := (self, o, opts) >> o;


DefaultCodegen.NewScalarProduct := meth(self, o, y, x, opts) 
	local itr, num_nz;
	#itr := o.rv.element.object.get_iterator();
	#num_nz := o.rv.element.object.num_nonzeros("sa2");
	#return decl([itr], chain(
	return decl([], chain(
		assign(Y, V(0))
		#loopw(lt(itr, num_nz),
		#chain(
	#))
	));
	end;

#if1(neq(struct_nth(x, "value", itr), V(0)), chain(
#assign(y, add(deref(y), mul(

DefaultCodegen.DenseScalarProduct := meth(self, o, y, x, opts)
	local arr, itr, size;
	arr := var.fresh_t("arr", TArray(o.rv.element.object.t.t, o.rv.element.object.size));
	itr := var.fresh_t("itr", TInt);
	return data(arr, V(o.rv.element.tolist()), decl([], chain(
		assign(y, V(0)),
		assign(itr, V(0)),
		loopw(lt(itr, o.rv.element.object.size),
		chain(
			if1(neq(nth(arr, itr), V(0)), chain(
			assign(y, add(deref(y), mul(nth(arr, itr), nth(x, itr)))),
			assign(itr, add(itr, V(1))))))))));
	end;

DefaultCodegen.SparseScalarProduct := meth(self, o, y, x, opts)
	local arr, itr, size;
	arr := var.fresh_t("arr", TArray(o.rv.element.object.t.t, o.rv.element.object.size));
	itr := var.fresh_t("itr", TInt);
	return data(arr, V(o.rv.element.tolist()), decl([], chain(
		assign(y, V(0)),
		assign(itr, V(0)),
		loopw(lt(itr, o.rv.element.object.size),
		chain(
			if1(logic_and(neq(nth(arr, itr), V(0)),neq(nth(x, itr), V(0))), chain(
			assign(y, add(deref(y), mul(nth(arr, itr), nth(x, itr)))),
			assign(itr, add(itr, V(1))))))))));
	end;

DefaultCodegen.TSparse := meth(self, o, y, x, opts) 
	local row, values;
	row := var.fresh_t("row", TArray(o.t, o.size1));
	values := var.fresh_t("values", TArray(o.t, o.size2));
	return data(row, o.list1, skip());
	end;

DefaultCodegen.sparse_nth2 := meth(self, o, y, x, opts) 
	local row, index;
	row := var.fresh_t("row", TArray(o.sa.t, o.sa.size));
	index := var.fresh_t("index", TInt);
	return decl([row, index], chain(
	if4(eq(deref(row + index), V(0)), assign(y, V(0)), assign(y, deref(row+index)))));
	end;


#CUnparser.TSparse := (self, o, i, is) >> Print(Blanks(i),
#	o.t, "row[", o.size1, "];\n", o.t, "values[", o.size2, "];\n");



SparseDefaults := CopyFields(SpiralDefaults, rec(
  compileStrategy := GraphIndicesCS,
  X := var("sa", TPtr(TSparse(TArray(TInt,1), TSemiring_Arithmetic, 1))),
  XType := TPtr(TSparse(TArray(TInt,1), TSemiring_Arithmetic, 1)),
  arrayDataModifier := "",
  arrayBufModifier := "",
  Y := var("res", TPtr(TInt)),
  YType := TPtr(TInt),
  isCSR := true,
  includes := ["<sparse.h>"],
  #symbol := ["struct_array sa2"]
));

