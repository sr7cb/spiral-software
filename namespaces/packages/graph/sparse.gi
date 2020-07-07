#Class(sparse_nth3, BaseOperation, rec(
#    __call__ := (self,x, list, size) >> WithBases(self,
#        rec(operations := NthOps,
#            x := x,
#			list := list,
#			size := size)),
#	  
#	  eval := meth(self)
#		local result;
#		result := var.fresh_t("result", TInt);
#		return decl[result]
#
#));

Class(TPair, CompositeTyp, rec(
	__call__ := (self) >> WithBases(self,
	rec( operations := TypOps)),
    print := self >> Print(self.name),
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
    __call__ := (self, datavar, len, ofs) >> WithBases(self, rec(
    var := datavar,
    operations := PrintOps,
    ofs := toExpArg(ofs),
    len := Checked(IsPosIntSym(len), len)
    )),

# <-Daniele's changes
#    rChildren := self >> [ self.var, self.len, self.ofs],
#    rSetChild := rSetChildFields("var", "len", "ofs"),

    rChildren := self >> [ self.var, self.len, self.ofs, self._domain, self._range],
    rSetChild := rSetChildFields("var", "len", "ofs", "_domain", "_range"),
    from_rChildren := (self, rch) >> ObjId(self)(rch[1], rch[2], rch[3]).setDomain(rch[4]).setRange(rch[5]),
# ->

    domain := self >> self.len,
    print := self >> Print(self.name,"(",self.var,", ",self.len,", ",self.ofs,")"),

    at := (self, n) >> When(IsInt(n) and IsValue(self.ofs) and IsBound(self.var.value),
        self.var.value.v[n + self.ofs.v + 1],
        nth(self.var, n + self.ofs)),

    tolist := self >> List([0..EvalScalar(self.len-1)], i -> nth(self.var, self.ofs+i)),
    lambda := self >> let(x := Ind(self.domain()), Lambda(x, nth(self.var, self.ofs+x))),

    domain := self >> self.len,
    range := self >> When(self._range=false, self.var.t.t, self._range),
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
       object := Cond(Length(arg) = 3, When(IsArrayT(arg[3]), arg[3], V(0))),
	   _val := Cond(Length(arg)=3, When(IsList(arg[2]), arg[2], [arg[2]]),
                               Drop(arg, 1)),
       val := When(Length(_val)=1 and IsLoc(_val[1]), _val[1], V(_val)),
       datavar := Cond(IsLoc(val), val,
                   Dat(val.t).setValue(val)),
       WithBases(self, rec(var := datavar, object := object, operations := PrintOps))),

   print := self >> Print(self.name, "(", self.var, ")"),
   rChildren := self >> [ self.var ],
   rSetChild := rSetChildFields("var"),

   iterator := 1,

   next := meth(self) 
	local result;
	if self.iterator < Length(self.tolist()) then
	  result := self.tolist()[self.iterator];
	  self.iterator := self.iterator + 1;
	fi;
	return result;
	end,

   tolist3 := self >> When(IsBound(self.var), self.var, self.lambda3().tolist3()),
   lambda3 := self >> let(x := Ind(self.domain()), Lambda(x, sparse_nth(self.var, x))),
   at := (self, n) >> When(IsInt(n), self.var.value.v[n+1], self.lambda().at(n)),
   at2 := (self, n) >> When(IsInt(n), self.var2.value.v[n+1], self.lambda().at(n)),
   tolist := self >> When(IsBound(self.var.value), self.var.value.v, self.lambda().tolist()),
   tolist2 := self >> When(IsBound(self.var2.value), self.var2.value.v, self.lambda2().tolist()),
   lambda := self >> let(x := Ind(self.domain()), Lambda(x, nth(self.var, x))),
   lambda2 := self >> let(x := Ind(self.domain()), Lambda(x, nth(self.var2, x))),

   domain := self >> self.var.t.size,
   range := self >> self.var.t.t,

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

Class(TSparse, TArrayBase, rec(
#__call__ := (self, t, size1, list1, size2, list2, sa) >>
#         WithBases(self, rec(
#         t    := Checked(IsType(t), t),
#         size1 := Checked(IsPosInt0Sym(size1), size1),
#		 list1 := [list1],
#         size2 := Checked(IsPosInt0Sym(size2), size2),
#		 list2 := [list2],
#		 sa := sa,	
#		operations := TypOps)),

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

CUnparser.if4 := (self,o,i,is) >> Print(Blanks(i),
    "if (", self(o.if_cond,i,is), ") {\n", self(o.if_cmd,i+is,is), Blanks(i), "}",
    " else {\n", self(o.else_cmd,i+is,is), Blanks(i), "}\n");

DefaultSumsGen.TSparse := (self, o, opts) >> o;

DefaultSumsGen.sparse_nth2 := (self, o, opts) >> o;

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


CUnparser.TSparse := (self, o, i, is) >> Print(Blanks(i),
	o.t, "row[", o.size1, "];\n", o.t, "values[", o.size2, "];\n");
