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


Declare(IsSparseT);

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
  t := 0;
  i := 1;
  if IsSparseT(S.data_type) then
	M := NullMat(S.data_type.dims()[1], S.data_type.dims()[2]);
	while t < S.data_type.size()-1 do 
		Print(t);
		if t = S.element[i][1] then
			M[1][t+1] := S.element[i][2];
			t := t + 1;
			i := i + 1;
		else
			M[1][t+1] := 0;
			t := t + 1;
		fi;
  	od;
  fi;
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


#IsSparseOfs := o -> IsRec(o) and IsBound(o.isSparseOfs) and o.isSparseOfs;

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
       ofs := toExpArg(arg[4]),
	   WithBases(self, rec(var := object, len := len, ofs := ofs, operations := PrintOps))),

   print := self >> Print(self.name, "(", self.var, ", ", self.len, ", ", self.ofs, ")"),
   rChildren := self >> [ self.var, self.len, self.ofs ], #DOMAIN AND RANGE BROKEN
   rSetChild := rSetChildFields("var", "len", "ofs" ),

# <-Daniele's changes
#    rChildren := self >> [ self.var, self.len, self.ofs],
#    rSetChild := rSetChildFields("var", "len", "ofs"),

#    rChildren := self >> [ self.var, self.len, self.ofs, self._domain, self._range],
#    rSetChild := rSetChildFields("var", "len", "ofs", "_domain", "_range"),
#    from_rChildren := (self, rch) >> ObjId(self)(rch[1], rch[2], rch[3]).setDomain(rch[4]).setRange(rch[5]),
# ->

    #domain := self >> self.len,

    #at := (self, n) >> When(IsInt(n) and IsValue(self.ofs) and IsBound(self.var.value),
    #   self.var.value.v[n + self.ofs.v + 1],
    #    nth(self.var, n + self.ofs)),
	#
    #tolist := self >> List([0..EvalScalar(self.len-1)], i -> nth(self.var, self.ofs+i)),
    #lambda := self >> let(x := Ind(self.domain()), Lambda(x, nth(self.var, self.ofs+x))),

	at := (self, n) >> When(IsInt(n) and IsValue(self.ofs) and IsBound(self.var.value),
       self.var.value.v[n + self.ofs.v + 1],
        sparse_nth3(self.var, n + self.ofs)),
	
    tolist := self >> List([0..EvalScalar(self.len-1)], i -> sparse_nth3(self.var, self.ofs+i)),
    lambda := self >> let(x := Ind(self.domain()), Lambda(x, sparse_nth3(self.var, self.ofs+x))),



	range := self >> When(self._range=false, self.var.t, self._range),
    domain := self >> self.len,
    #range := self >> When(self._range=false, self.var.t.t, self._range),
    inline := true,
    free := self >> self.ofs.free()
));

#F FDataSparse(<datavar>) -- symbolic function i -> datavar[i],
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

Class(FDataSparseMat, Function, rec(
	__call__ := arg >> let(
       self := arg[1],
       object := arg[2],
       var := Cond(IsList(arg[3]), arg[3], [arg[3]]),
	   WithBases(self, rec(var := var, object := object, operations := PrintOps))),
	
	print := self >> Print(self.name, "(", self.object, ", ", self.var, ")"),
	rChildren := self >> [self.var],
	rSetChild := rSetChildFields("var"),

	at := (self, n) >> self.lambda().at(n),
	tolist := self >> self.lambda().tolist(),
	#lambda := self >> let(k := Ind(object.expr.loc.loc.dims[1]))
	lambda := self >> let(k := Ind(self.domain()), u := Ind(self.domain()), Lambda(k, Lambda(u, self.object.at(k,u)))),

	domain := self >> Length(self.var),
	range := self >> self.object.t.params[1],
));

Class(FDataSparseMatOfs, Function, rec(
	__call__ := arg >> let(
       self := arg[1],
       object := arg[2],
	   len := arg[3],
       ofs := arg[4],
	   WithBases(self, rec(object := object, len := len, ofs := ofs, operations := PrintOps))),
	
	print := self >> Print(self.name, "(", self.object, ", ", self.len, ", ", self.ofs, ")"),
   	rChildren := self >> [ self.object, self.len, self.ofs ],
   	rSetChild := rSetChildFields("object", "len", "ofs"),


	at := (self, n) >> When(IsInt(n) and IsValue(self.ofs) and IsBound(self.object.value),
       self.object.value.v[n + self.ofs.v + 1],
        nth(self.object, self.lambda().at(n))),
	
	#tolist := self >> self.lambda().tolist(),
	tolist := self >> List([0..EvalScalar(self.len-1)], i -> nth(self.object, self.ofs+i)),
	lambda := self >> let(k := Ind(self.domain()), u := Ind(self.domain()), Lambda(k, Lambda(u, self.object.at(k,u)))),

	range := self >> self.object.t,
    domain := self >> self.len,
	inline := true,
    free := self >> self.ofs.free()
));



Declare(TSparse);
Declare(Ttrace);



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


#Class(GathPtr, Gath, rec(
#    rChildren := self >> [self.ptr, self.func],
#    rSetChild := rSetChildFields("ptr", "func"),
#    new := (self, ptr, func) >> SPL(WithBases(self, rec(
#        ptr := ptr,
#      	func := Checked(IsFunction(func) or IsFuncExp(func), func)))).setDims()
#));


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
		t := Cond(IsPtrT(loc) or IsSparseT(loc.t), loc.t.t, loc.t),
		operations := NthOps
	)),
	rChildren := self >> [self.loc, self.elem, self.idx],
	rSetChild := rSetChildFields("loc", "elem", "idx"),
	#print := (self, i, si) >> Print(self.name, "
));

spiral.spl.RowVec.traversal := (self, sa, body) >> let(
			itr := var.fresh_t("itr", TInt),
			loopw(neq(struct_nth(sa, "index", itr), V(0)), chain(
				body
			))
	);

Class(RowVec2, RowVec, rec(
	init := self >> let(
		result := var.fresh_t("result", TInt),
		assign(result, V(0))
	),

	traversal := (self, sa, body) >> let(
			itr := var.fresh_t("itr", TInt),
			loopw(neq(struct_nth(sa, "index", itr), NULL(TInt)), chain(
				body
			))
	),
));


Class(DAGNode, TaggedNonTerminal,  rec(
    abbrevs :=  [ (nt, ylist, xlist) -> Checked(IsSPL(nt), [nt, When(IsList(ylist), ylist, [ylist]), When(IsList(xlist), xlist, [xlist])]) ],
    #transpose := self >> ObjId(self)(self.params[1].transpose(), self.params[2], self.params[3]).withTags(self.getTags())
));

Class(DAG, TCompose, rec(
    terminate := self >> Error("Not yet implemented."),
    
    from_rChildren := (self, rch) >> let(
        len := Length(rch),
        transposed := rch[len-1],
        tags := rch[len],
        t := ApplyFunc(ObjId(self), [rch{[1..len-2]}]),
        tt := When(transposed, t.transpose(), t),
        attrTakeA(tt.withTags(tags), self)
    ),

    rChildren := self >>
        Concatenation(self.params[1], [self.transposed, self.tags]),

    rSetChild := meth(self, n, newChild)
        local l;
        l := Length(self.params[1]);
        if n <= l then
            self.params[1][n] := newChild;
        elif n = l+1 then
            self.transposed := newChild;
        elif n = l+2 then
            self.tags := newChild;
        else Error("<n> must be in [1..", l+2, "]");
        fi;
        # self.canonizeParams(); ??
        self.dimensions := self.dims();
    end
    
));

INF := arg -> Cond(
   Length(arg)=0, Int(1000000),
   Length(arg)=1, var.fresh("i", TInt, toRange(Int(1000000))),
   Error("Usage: INF() | INF(<range>)")
);


#Class(INF, Value, rec(
#	__call__ := self >> SPL(WithBases(self, rec(
#		t := TInt,
#		v := V(1),
#		operations := ValueOps,
#	))),
#	isSPL := true,
#	dims := self >> self.v,
#	eval := self >> self,
#	print := (self, i, si) >> Print(self.name),
#));

IsMaskT := x ->  IsBound(x.isMaskT) and x.isMaskT;

Class(Mask, Diag, rec(
	__call__ := (self, func) >> SPL(WithBases(self, rec(
		element := func,
		var := func.var,
	))),
	isMaskT := true,
	print := (self, i, si) >> Print(self.name, "(", self.element, ")"),
	index := (self, i, j, dom) >> nth(self.var, add(mul(dom, i), j)),
));

Declare(SPLScope);


Class(SPLScope, Buf, rec(

	__call__ := (self, spl, scope) >> SPL(WithBases(self, rec(
		spl := Checked(IsSPL(spl), spl),
		scope := scope,
		_children := [spl],
		dimensions := spl.dimensions,
	))),
	dims := self >> Dimensions(self.spl),
	rChildren := self >> [self.spl, self.scope],
	rSetChild := rSetChildFields("spl", "scope"),
	#append := (self, L) >> Union(self.list, L),

	#freshScope := (self, inVar, outVar) >> SPL(WithBases(self, rec(
	#		input := inVar,
	#		output := outVar,
	#		_children := [],
	#	))),

	freshScope := (self) >> var.fresh_t("t", TPtr(TReal)),
));


Class(SpContainer, SumsBase, BaseContainer, rec(
	new := (self, spl) >> SPL(WithBases(self, rec(
		_children := [spl]))).setDims(),
	
	dims := self  >> self.child(1).dims()
));


Declare(TSparse_Matrix);

Class(HyperSprase, BaseOperation, rec(
	__call__ := arg >> let(
    self := arg[1],
    vector := arg[2],
	matrix := arg[3],
	WithBases(self, rec(vector := vector, matrix := matrix, _children := [vector, matrix], operations := PrintOps))),
	
   	isSparseT := true,
   	print := self >> Print(self.name, "(", self.vector, ", ", self.matrix, ")"),
   	dims := self >> self.vector.dims(),

));

Class(MatMul, BaseMat, rec(
	__call__ := (self, dimX, dimY) >> SPL(WithBases(self, rec(
		dimX := dimX,
		dimY := dimY,
		dimensions := [dimX*dimY, dimY*dimX],
	))),
	isPermutaiton := False,
	isReal := True,

	rChildren := self >> [self.dimX, self.dimY],
	rSetChild := rSetChildFields("dimX", "dimY"),

	dims := self >> self.dimensions,
	print :=  (self, i, si) >> Print(self.name, "(", self.dimX, ", ", self.dimY,")"),

));

Class(MakeDiag, Diag, rec(
	dims := self >> [self.element.domain(), self.element.domain()],
));


Class(Reduce, BaseMat, rec(
	__call__ := (self, traversal, size, t) >> SPL(WithBases(self, rec(
		traversal := Checked(IsString(traversal), traversal),
		size := size,
		t := Checked(IsType(t), t),
		dimensions := Cond(traversal = "Col", [1, size], [size, 1]),
	))),
	
	rChildren := self >> [self.traversal, self.t],
	rSetChild := rSetChildFields("traversal", "t"),

	dims := self >> self.dimensions,

	print :=  (self, i, si) >> Print(self.name, "(", self.traversal, ", ", self.size, ", ", self.t, ")"),

	isPermutaiton := False,
	isReal := True,

));

Class(SparseBlk, SumsBase, Mat, rec(
	new := (self, M) >> SPL(WithBases(self, rec(
			data_type := M.object,
            element := M.var,
            TType   := Cond( # NOTE: add checks to M
                            IsList(M.var),     UnifyTypes(List(Flat(M.var), InferType)),
                             IsValue(M.var),    M.t.t,
                           IsSymbolic(M.var), M.t.t),
                       ))).setDims(),
    area := self >> Length(Filtered(Flat(self.element), k -> k<>0)),
    new  := (self, M) >> SPL(WithBases(self, rec(element := M))).setDims(),
    dims := self >> Dimensions(self.element)
));


#Class(Trianglecount, TaggedNonTerminal, rec(
#	abbrevs := [(n) -> [n]],
#	isReal := self>>true,
#	dims := self >> [self.params[2]
#))

Declare(RulesSPLScope);
Declare(RulesMask);

Class(SpMV, TaggedNonTerminal, rec(
	abbrevs := [(n) -> [n]], 
));
Class(TVStack, TaggedNonTerminal, rec(
	abbrevs := [(n) -> [n]],
));
Class(TRowVec, TaggedNonTerminal, rec(
	abbrevs := [(n) -> [n]], 
));

Class(TSparse_Mat, TaggedNonTerminal, rec(
	abbrevs := [(n) -> [n]],
	isReal := self>>true,
	dims := self >> [self.params[1], self.params[1]]
)); 

Class(Trace, TaggedNonTerminal, rec(
	abbrevs := [(n) -> [n]],
	isReal := self >> true,
	dims := self >> self.params[1].dims()[1] * self.params[1].dims()[2],
	print := (self, i , si) >> Print(self.name, "(", self.params[1], ")")
));

Class(MatrixMultiply, TaggedNonTerminal, rec(
	abbrevs := [(n,n, p) -> [n,n, p]],
	isReal := self >> true,
	dims := self >> self.params[1] * self.params[2],
	print := (self, i , si) >> Print(self.name, "(", self.params[1], ", ", self.params[2], Checked(self.params[3] <> "", Print(", ", self.params[3])), ")")
));

NewRulesFor(SpMV, rec(
	column_reduce := rec(
			info := "Column reduction based spmv",
			maxSize := false,
			applicable := (self, nt) >> true,
	)
));

#NewRulesFor(MatrixMultiply, rec(
#	 matmul := rec(
#		 info := "Matrix Multplication",
#		 maxSize := false,
#		 applicable := (self, nt) >> Cond(nt.params[3] = "", true, false),
#		 apply := (nt, c, cnt) -> let(i := Ind(3), j := Ind(3), scat := Scat(fTensor(fBase(i), fBase(j))), 
#		 gath := Gath(fStack(fTensor(fBase(i), fId(3)), fTensor(fId(3), fBase(j)))), scp := SPLScope.freshScope(),
#		 kernel := scat * SPLScope(RowVec(FDataOfs(scp,6,V(3))), scp)  * gath, kernel2 := Rewrite(kernel, RulesSPLScope, opts), ISum(i, 3, ISum(j, 3, kernel2)))
#	 ),
#	 maskedmxm := rec(
#		 info := "Masked Matrix Multplication",
#		 maxSize := false,
#		 applicable := (self, nt) >> Cond(IsSPL(nt.params[3]), IsMaskT(nt.params[3]), false),
#		 apply := (nt, c, cnt) -> let(
#								i := Ind(3),
#								j := Ind(3),
#								scat := Scat(fTensor(fBase(i), fBase(j))), 
#								gath := Gath(fStack(fTensor(fBase(i), fId(3)), fTensor(fId(3), fBase(j)))),
#								scp := SPLScope.freshScope(),
#								kernel := scat * SPLScope(RowVec(FDataOfs(scp,6,V(3))), scp) * gath,
#								mxm := ISum(i, 3, ISum(j, 3, kernel)),
#								maskedmxm := Mask(FDataOfs(M, 9, 0)) * mxm,
#								maskedmxm2 := Rewrite(maskedmxm, RulesMask, opts),
#								maskedmxm3 := Rewrite(maskedmxm2, RulesSPLScope, opts), maskedmxm3)
#	 )
#));


NewRulesFor(Trace, rec(
	csr_trace := rec(
		info := "Trace of CSR matrix",
		maxSize := false,
		applicable := (self, nt) >> true,#nt.params[1].dims()[1] =1,
		#apply := (nt, c, cnt) -> let(i := Ind(4), ISumAcc(Ind(), 4, ScatAcc(fId(1)) * Gath(fTensor(fBase(i), fBase(i))))),
		apply := (nt, c, cnt) -> let(i := Ind(5), ISumAcc(i, 5, Gath(fTensor(fBase(i), fBase(i))))),
	)
));


NewRulesFor(TSparse_Mat, rec(
	csr := rec(
		info := "Vertical stack of Row Vectors",
		maxSize := false,
		applicable := (self, nt) >> nt.params[1].dims()[1] = 1,
		#apply := (nt, c, cnt) -> VStack(nt.params[1]),
		apply := (nt, c, cnt) -> TSparse_Matrix(nt.params[1]),
	),
	csc := rec(
		info := "Horizontal stack of Column Vectors",
		maxSize := false,
		applicable := (self, nt) >> nt.params[1].dims()[2] = 1,
		apply := (nt, c, cnt) -> HStack(nt.params[1]),
	)
));


Class(Ttrace, BaseOperation, rec(
	__call__ := (self, object) >> WithBases(self, rec(
        object := object,
        t := object.t,
        operations := TypOps)),

	print := self >> Print(self.name, "(", self.object, ")"),
	#size := self >> self.t.size,
	dims := self >> [1, 1],
	rChildren := self >> [self.object],
	rSetChild := rSetChildFields("object"),
));

IsSparseT := x -> IsType(x) and IsBound(x.isSparseT) and x.isSparseT;

Class(TSparse_Matrix, TArrayBase, rec(
   #__call__ := arg >> let( 
	#    size := arg[2],
   #     element := TArray(arg[1], arg[2]),
   #     t := arg[1].t,
	#	prop := Cond(IsList(arg[3]), arg[3], [arg[3]]),
   #     operations := TypOps,WithBases(self, rec()), 
	
	__call__ := arg >> let(
       self := arg[1],
       size := arg[2].size(),
	   element := TArray(arg[2], size),
       prop := Cond(IsList(arg[3]), arg[3], [arg[3]]),
	   WithBases(self, rec(size := size, element := element, prop := prop, _children := [arg[2], prop], operations := PrintOps))),
	
   	isSparseT := true,
   	print := self >> Print(self.name, "(", self.element, ", ", self.prop, ")"),
   	dims := self >> [self.size, self.element.size],

	rChildren := self >> [self.element.t.t, self.size],
	rSetChild := rSetChildFields("t", "size"),

	range := self >> self.element.t.t,

	traverse_outer := (self, i, j, var, body) >>  loopf(j, nth(var,i), nth(var, add(i, V(1))), chain(body)),
	index_row := (self, var, n, i) >> nth(var, add(n, add(V(1), i))),
	index_val := (self, var, n, j) >> nth(var, add(n, add(nth(var, n), add(j, V(1))))),
));


Declare(inref);

Class(TSparse, TArrayBase, rec(
	__call__ := (self, t, ring) >> 
			WithBases(self, rec(
			t    := Checked(IsType(t), t),
			ring := ring,
			size2 := Checked(IsPosIntSym(t.size), t.size),
			operations := TypOps)),
    isSparseT := true,
    vtype := (self, v) >> TSparse(self.t.vtype(v), self.size2/v),
    toPtrType := self >> TPtr(self.t),
    doHashValues := true,
    #dims := self >> Cond(
    #    ObjId(self.t)=TSparse, [self.size] :: self.t.dims(),
    #    [self.size]),
	size := self >> self.size2,
	dims := self >> [1, self.size2],
	rChildren := self >> [self.t, self.ring],
	rSetChild := rSetChildFields("t", "ring"),
#	print := self >> Print(self.__name__, "(", self.t, ", ", self.size1, ", ", self.list1, ", ", self.size2, ", ", self.list2, ", ", self.sa, ")"),	
	range := self >> self.t,
	print := self >> Print(self.__name__, "(", self.t, ", ", self.ring, ")"),

    get_var := self >> let(var.fresh_t("spr_arr", TPtr(TSparse(TArray(TInt,5), TSemiring_Arithmetic(TInt))))),
	
	length := (self, x) >> struct_nth(inref(x), "length", ""),

	get_elem_index := (self, x, idx) >> struct_nth(inref(x), "index", idx),

	get_elem_value := (self, x, idx) >> struct_nth(inref(x), "value", idx),

	traversal := (self, i, low, high, body) >> loopf(i, low, high, chain(body)),

	#num_nonzeros := (self, x) >> let( 
	#	itr := var.fresh_t("itr", TInt),
	#	result := var.fresh_t("result", TInt),
	#	decl([itr, result], chain(
	#	assign(result, V(0)),
	#	loopw(neq(struct_nth(x,"value", itr), NULL(TInt)), chain(
	#	if1(neq(struct_nth(x,"value", itr), V(0)), chain(
	#		assign(result, add(result, V(1)))))))))),
));


Class(fItrStack, FuncClassOper, rec(
	__call__ := meth(arg)
	 local self, children, lkup, res, h;
        self := arg[1];
        children := Flat(Drop(arg, 1));
        if self.skipOneChild and Length(children)=1 then return children[1]; fi;

        h := self.hash;
        if h<>false then
            lkup := h.objLookup(self, children);
            if lkup[1]<>false then return lkup[1]; fi;
        fi;
        res := WithBases(self, rec(operations := RewritableObjectOps, _children := children));
        if h<>false then return h.objAdd(res, lkup[2]);
        else return res;
        fi;
    end,

	domain := self >> self._children[2].domain() * self._children[1].range,
    range := self >> self._children[2].range(),  
    subdomainsDivisibleBy := (self, n) >> ForAll(self._children, x -> x.domain() mod n = 0),

    #lambda := meth(self)
    #    #local vals;
    #    #vals := [];
	#	for i in [1..self.domain()] do 
	#	Add(vals, self.child(2).lambda().at(add(idiv(sub(i,V(1)), add(self.range(),V(1))), mul(self.range(), imod(sub(i,V(1)), add(self.range(), V(1)))))));
	#	od;
	#	return Lambda(self._children[1], vals);
    #end

	lambda := meth(self)
		local v, j;
		v := Ind(self.range());
		#return Lambda(v, self.child(2).lambda().at(add(idiv(v, idiv(self.range(), self.domain())), mul(self.range(), imod(v, idiv(self.range(), self.domain()))))));
		return let(l := Lambda(v, self.child(2).lambda().at(imod(v, self._children[2].domain()))), 
					#c := Collect(l, @(1,var, e -> e.range = self._children[2].domain())),
					#c := Collect(l, @(1, add, e -> Error())),
					s := SubstVars(Copy(l), rec((self.child(1).id) := idiv(v, self.child(2).domain()))),
					s);
	end
));

Class(RulesSPLScope, RuleSet);
Class(RulesINF, RuleSet);
Class(RulesSymbol, RuleSet);
Class(RulesTrace, RuleSet);
Class(RulesMG, RuleSet);
Class(RulesMR, RuleSet);
Class(RulesMR2, RuleSet);
Class(RulesMask, RuleSet);
Class(RulesIDiag, RuleSet);
Class(RulesMatMul, RuleSet);


RewriteRules(RulesIDiag, rec(
	convert_I := ARule(SUM, [@(1,I), @(2, MakeDiag)], e-> [let(j := Ind(@(1).val.obj.size), ivs := IterVStack(j, Gath(fTensor(fId(j.range), fBase(j)))), SUM(ivs, @(2).val))]),
));

RewriteRules(RulesMatMul, rec(
	consume_matmul := ARule(Compose, [@(1,MatMul), [@(2,SUM), @(3,IterVStack), @(4,MakeDiag)]], e->[Error()]),
));
#comments for rulesmxmtrace
#cis := Rule([ISum, [ISum, @(1)]], e -> [let(rec(result := ISumAcc(e.var, e.domain, @(1).val)), result)]),
#remove_trace := ARule(Compose, [@(1, RowVec), @(2, Gath), @(3,ISum)], e->[@(3).val]),
#change_isum_isumacc := ARule(ISum, [@(1, SPLScope)], e -> [let(updom := e.var, inputs := e._children[1], isa := ISumAcc(updom, updom.range, inputs), isa)]),
#change_move_scat := ARule(ISum, [@(1, SPLScope)], e -> [Error()]),
#remove_trace := ARule(Compose, [@(1, RowVec), @(2, Gath), @(3,ISum)], e->[@(3).val]),
#remove_trace := ARule(Compose, [@(1, Gath), [@(2, ISum), @(3, ISum, e -> let(s := Collect(Copy(e._children[1]._children), Scat), s[1].func.__name__ = "fTensor"
	#																			and Length(s[1].func._children) = 2 and s[1].func._children[1].__name__ = "fBase" 
	#																			and s[1].func._children[2].__name__ = "fBase"))]], 
	#																			e->[let(v1 := @(2).val.var, v2 := @(3).val.var, f := fBase(@(2).val.var), c := @(3).val._children[1], two := @(3).val._children[1]._children[2] * @(3).val._children[1]._children[3], 
	#																			ISum(v1, v1.range, ISum(v2, v2.range, COND(fBase(v1), Scat(fBase(v1)) * two, O(c.dims()[1],1)))))]),
RewriteRules(RulesMG, rec(
	move_gath := ARule(Compose, [@(1, Gath), [@(2, ISum), @(3, ISum, e -> let(s := Collect(Copy(e._children[1]._children), Scat), Length(s) = 1))]], 
																							e -> [let(exp := @(1).val * @(3).val._children[1], 
																									ISum(@(2).val.var, @(2).val.var.range, 
																									ISum(@(3).val.var, @(3).val.var.range,
																								  	exp)))]),

	gath_scat_cond := ARule(Compose, [@(1, Gath), @(2, Scat)], e->[let(v1 := @(2).val.func._children[1].params[2], v2 := @(2).val.func._children[2].params[2], 
																		COND(eq(v1, v2), 
																			Scat(@(2).val.func._children[1]), 
																			O(@(2).val.func._children[1].params[1], 1)))]),	
));


RewriteRules(RulesMR, rec(
	collapse_loop_cond := Rule([@(1,ISum), [@(2,ISum), [@(3, Compose), @(4,COND),...]]], e->let(v1 := @(1).val.var, v2 := @(2).val.var, com := @(3).val,
																								com2 := SubstTopDown(Copy(com), @(5, COND), g -> @(5).val._children[1]),
																								com3 :=  SubstVars(Copy(com2), rec((v2.id) := v1)),
																								ISum(v1, v1.range, com3))),
));

RewriteRules(RulesMR2, rec(
	move_rowvec := ARule(Compose, [@(1, RowVec), [@(2, ISum), [@(3, Compose), @(4,Scat), ...]]], e -> [let(v1 := @(2).val.var, s := @(3).val._children[2], f := @(3).val._children[3],
																											rv := RowVec(fCompose(@(1).val.element, @(4).val.func)),
																											ISumAcc(v1, v1.range, rv * s * f))]),
));

#e->[let(v1 := @(2).val.var, v2 := @(3).val.var, 
#inside := @(3).val._children[1], dom := @(2).val.domain,
#con := COND(eq(@(1).val.index(v1,v2,dom), 1)))]),
RewriteRules(RulesMask, rec(
	move_mask := ARule(Compose, [@(1,Mask), [@(2, ISum), @(3, ISum)]], e -> [let(exp := @(1).val * @(3).val._children[1],
																			ISum(@(2).val.var, @(2).val.var.range, 
																			ISum(@(3).val.var, @(3).val.var.range,
																			exp)))]), 
	replace_mask := Rule([@(1,ISum), [@(2,ISum), [@(3, Compose), @(4,Mask),...]]], e -> let(v1 := @(1).val.var, v2 := @(2).val.var, dom := @(2).val.domain,
																	inside := @(3).val, inside2 := SubstTopDown(Copy(inside), @(5, Mask), g -> I(@(5).val.element.len)),
																	con := COND(eq(@(4).val.index(v1,v2,dom), 1), inside2, Scat(fTensor(fBase(v1), fBase(v2))) * O(1,1)), ISum(v1, v1.range, ISum(v2, v2.range, con)))),
));

#comments for rulestrace	
#change_fbase := ARule(fCompose, [@(1,itrfStack, e -> let(one := e._children[2]._children[1].domain(), two := e._children[2]._children[2].domain(), one = two)), @(2, fBase)], e -> [let(c := Collect(e, var), v2 := c[1], SubstTopDown(e, @(1,var, e-> var.range <> v2.range), e->v2))]),#[fCompose(@(1).val, @(1).val._children[2]._children[1])]),
RewriteRules(RulesTrace, rec(
	change_fbase := ARule(Gath, [@(1, fCompose, e -> let(length := Length(e._children), length = 2))],
									 e -> [let(c := Collect(e, var), 
												v2 := c[Length(c)],
												result := SubstTopDown(Copy(e), @(1, var, e-> var.range <> v2), e -> v2),
												Error())]),
));

#comments for RulesScope
#newspl2 := SubstTopDown(Copy(newspl), @(1, var, p -> p = s.scope), e -> X),
#rowvec_gath := ARule(Compose, [@(1, RowVec), @(2, Gath)],e -> [let(i := Ind(@(1).val.element._children[1].len), ISum(i, e._children[2].element._children[1].len, diagMul(e._children[2].element,e._children[3].func)))]),
#[let(scat := e._children[1], s := e._children[2], g := e._children[3].func, newscope := SPLScope(RowVec(diagMul(fCompose(s.spl.element._children[1], g._children[2]))), s.scope), newscope * Gath(g._children[1]))]),
RewriteRules(RulesSPLScope, rec(
	move_inside_gath := ARule(Compose, [@(1, SPLScope, e-> 
		let(check1 := Collect(e.spl, FDataOfs), check2 := Collect(e.spl, @@(1, var, (s, cx) -> s = e.scope and cx.FDataOfs[1].var <> s)), Length(check1) > 0 and Length(check2) = 0)), 
		[@(2,Gath, e-> Length(e.func._children) = 2 and e.func._children[1].domain() = e.func._children[2].domain()), @(3, fStack)]], 
			e -> [let(scat := e._children[1], 
					s := e._children[2], 
					g := e._children[3].func, 
					fdataofs := FDataOfs(s.spl.element.var, s.spl.element.len/2, V(0)),
					newspl := RowVec(fCompose(fdataofs, g._children[2])),
					newscope := SPLScope(newspl * Gath(g._children[1]), s.scope),
					newscope)]),	

	move_inside_scat := ARule(Compose, [@(1, Scat), @(2, SPLScope)], e-> [SPLScope(@(1).val * @(2).val.spl, @(2).val.scope)]),
));

RewriteRules(RulesStrengthReduce, rec(
	add := ARule(add, [@1, INF()], e -> INF),
	fTen := ARule(fTensor, [@(1, Ind(INF()))], e -> V(1)),
	fBas := ARule(fBase, [@(1, Ind(INF()))], e -> V(1)),
	fI := ARule(fId, [@(1, Ind(INF()))], e -> V(1)),
	fSta := ARule(fStack, [@(1, Ind(INF()))], e -> V(1)),
	#Ind := ARule(Ind, [@(1, INF)], e -> V(1)),
	#fdo := ARule(FDataofs, [@(1, INF)], e -> V(1))
));

#RewriteRules(RulesSymbol, rec(
#	addsymbol := Cond(IsBound(opts.symbol), SubstTopDown(Copy(cs), [@(1,nth), ..., @(2,var, e-> e=X), ...], 
#    e -> nth(opts.symbol[1], e.idx)), cs);
#));


Class(T_Sparse, T_Type, rec(
       t := TInt,
));

Class(NULL, Command, rec(
__call__ := (self, t) >> WithBases(self, rec(
	t    := Checked(IsType(t), t),
    operations := CmdOps
  )),
 	rChildren := self >> [],
	print := (self, i, si) >> Print(self.__name__)
));

Class(inref, nth, rec(
	__call__ := (self, loc) >> Inherited(loc, TInt.value(0)),
    rChildren := self >> [self.loc],
    rSetChild := rSetChildFields("loc"),
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

CUnparser.inref := (self, o, i, is) >> Print("(*", self(o.loc, i, is), ")");

CUnparser.TSparse := (self,t, vars,i,is) >> Print(Blanks(i),
	"struct sparse_arr", self.infix(vars, ", ", i + is));

#CUnparser.sparse_nth3 := (self, o, i, is) >> o.list.get_elem_value(o.list.get_var(), o.idx);

CUnparser.struct_nth := (self,o,i,is) >> Cond(o.idx <> "", Print(Blanks(i),
	self(o.loc,i,is), ".", o.elem, "[", o.idx, "]"), Print(Blanks(i),
	self(o.loc,i,is), ".", o.elem));

CUnparser.if4 := (self,o,i,is) >> Print(Blanks(i),
    "if (", self(o.if_cond,i,is), ") {\n", self(o.if_cmd,i+is,is), Blanks(i), "}",
    " else {\n", self(o.else_cmd,i+is,is), Blanks(i), "}\n");

CUnparser.NULL := (self, o, i, is) >> Print(Blanks(i), "NULL");

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

DefaultSumsGen.sparse_nth3 := (self, o, opts) >> o.element.get_elem_value(o.element.get_var(), o.idx);

#DefaultSumsGen.RowVec := (self, o, opts) >> let(n := Ind(o.element._children[1].len), Error(), ISumAcc(n, o.element._children[1].len, ScatAcc(o.element)));
#let(c := Collect(self, @(1,var, e -> IsSparseT(e))), Length(c) > 0)
DefaultSumsGen.RowVec := (self, o, opts) >> Cond(let(c := Collect(o, TSparse), Length(c) > 0), o, let(i := Ind(o.element.domain()), ISumAcc(i, o.element.domain(), Scat(fId(1)) * Blk1(o.element.at(i))) * Gath(fBase(i))));

DefaultSumsGen.RowVec2 := (self, o, opts) >> o;

DefaultSumsGen.Ttrace := (self, o, opts) >> o;

DefaultSumsGen.TSpars_Matrix := (self, o, opts) >> o;

DefaultSumsGen.HyperSprase := (self, o, opts) >> o;

DefaultSumsGen.Reduce := (self, o ,opts) >> RowVec(fConst(o.t, o.size, 1));


DefaultSumsGen.IterVStack := (self, o, opts) >> Cond(let(c := Collect(o, TSparse), Length(c) > 0), o, let(
	bkcols := Cols(o.child(1)),
	bkrows := Rows(o.child(1)),
	nblocks := o.domain,
	cols := Cols(o), rows := Rows(o),
	ISum(o.var, o.domain,
	    Scat(fTensor(fBase(nblocks, o.var), fId(bkrows))) *
	    self(o.child(1), opts) *
	    Gath(fId(bkcols)))
    ));

DefaultSumsGen.SPLScope := (self, o, opts) >> SPLScope(self(o.child(1), opts), o.scope);

DefaultCodegen.SPLScope := (self, o, y, x, opts) >> 
	decl([o.scope], chain(assign(o.scope, x), self(o.child(1), y, x, opts)));
	
	#decl([o.scope], chain(assign(o.scope, x), self(o.child(1), o.scope, x, opts))); #return decl([o.scope], self(o.child(1)._children[1], o.scope.output, x, opts))); #(self(o.child(1), y, x, opts));
	#return decl([o.scope], chain(self(o.child(1)._children[2], o.scope, x, opts), self(o.child(1)._children[1], y, o.scope, opts))); #(self(o.child(1), y, x, opts));

#Overridden Codegen from SpiralDefaults
DefaultCodegen.ISumAcc := (self, o, y, x, opts) >> let(ii := Ind(), chain(loop(ii, Rows(o), assign(nth(y, ii), V(0))), loopf(o.var, V(0), o.domain, self._acc(self(o.child(1), y, x, opts), y))));


DefaultCodegen.IterVStack := meth(self, o, y, x, opts) 
	local v, i, j, itr; 
	v := o._children[1].element.var.get_var();
	itr := var.fresh_t("itr", TInt);
	i := Ind();
	j := Ind();
	return decl([v,i,j,itr], chain(assign(itr, V(0)), o._children[1].element.var.traversal(i, V(0), opts.symbol[1], 
				if1(eq(o._children[1].element.var.get_elem_index(v, itr), i), chain(  
					opts.matrix.traverse_outer(i,j,x,assign(nth(y, nth(x, add(opts.symbol[1], add(V(1), j)))), add(nth(y, nth(x, add(opts.symbol[1], add(V(1), j)))), 
					mul(opts.matrix.index_val(x,opts.symbol[1], j), o._children[1].element.var.get_elem_value(v, itr))))),
					assign(itr, add(itr, V(1))))))));
end;


DefaultCodegen.RowVec := (self, o, y, x, opts) >> Cond(let(c := Collect(o, TSparse), Length(c) > 0) and IsBound(opts.matrix), let(i := Ind(), j := Ind(),
v := o.element.var.get_var(), t := var.fresh_t("t", TInt),
decl([v, i, j, t], chain(assign(t, V(0)), assign(i, V(0)), assign(j, V(0)), loopw(logic_and(lt(i, o.element.var.length(v)), lt(j, o.element.var.length(x))), 
	if3(lt(o.element.var.get_elem_index(v, i), o.element.var.get_elem_index(x, j)),
			assign(i, add(i, V(1))), 
			lt(o.element.var.get_elem_index(x, j), o.element.var.get_elem_index(v, i)), 
			assign(j, add(j, V(1))), 
			chain(assign(t, mul(o.element.var.get_elem_value(v, i), o.element.var.get_elem_value(x, j))), 
			assign(i, add(i, V(1))), assign(j, add(j, V(1)))))), assign(nth(y, 0), t)))), 
let(c := Collect(o, TSparse), Length(c) > 0) and IsBound(opts.matrix) = false,
	let(i := Ind(), v := o.element.var.get_var(), t := var.fresh_t("t", TInt), 
		decl([v, i, t], chain(assign(t, V(0)), assign(i, V(0)), loopw(lt(i, o.element.var.length(v)), 
		chain(assign(t, mul(o.element.var.get_elem_value(v, i), nth(x,o.element.var.get_elem_index(v, i)))), 
		assign(i, add(i, V(1))))), assign(nth(y, 0), t)))), 
let(c := Collect(o, TSparse), Length(c) = 0) and IsBound(opts.matrix),
	let(i := Ind(), t := var.fresh_t("t", TInt), 
		decl([i, t], chain(assign(t, V(0)), assign(i, V(0)), loopw(lt(i, x.t.t.length(x)), 
		chain(assign(t, mul(x.t.t.get_elem_value(x, i), o.element.at(x.t.t.get_elem_index(x, i)))), 
		assign(i, add(i, V(1))))), assign(nth(y, 0), t)))),
let(i := Ind(),
   	func := o.element.lambda(),
   	t := TempVar(x.t.t),
  	chain(assign(t, 0), loop(i, func.domain(), assign(t, add(t, mul(func.at(i), nth(x, i))))), assign(nth(y, 0), t)))
);

#DefaultCodegen.RowVec2 := (self, o, y, x, opts) >> 

#	v := var.fresh_t("v", TArray(TInt, o.element.domain())),
#	t := TempVar(x.t.t),
#	decl([v,t], chain(assign(t, 0), 
#	loopw(lt(t, o.element.domain()), 
#	chain(if1(logic_and(neq(nth(v, i), V(0)), neq(nth(x,i), V(0))), assign(t, add(t, mul(nth(v,i), nth(x,i))))) , assign(nth(y,0), t)))))), 
#	let(i := Ind(),
#   func := o.element.lambda(),
#   	t := TempVar(x.t.t),
#  	chain(assign(t, 0), loop(i, func.domain(), assign(t, add(t, mul(func.at(i), nth(x, i))))), assign(nth(y, 0), t)))); 

DefaultCodegen.TSparse_Matrix := meth(self, o, y, x, opts)
    local i, j, k, n, b;
    i := Ind();
    j := Ind();
    k := Ind();
    n := var.fresh_t("n", TInt);
    b := var.fresh_t("b", TPtr(TInt));
    return decl([], chain(
            loopf(i, V(0), n, decl([], chain(
                    loopf(j, deref(add(x,i)), deref(add(x, add(i, V(1)))), decl([], chain(
                            loopf(k, nth(b, deref(add(x, add(n, add(V(1),j))))), nth(b, add(deref(add(x, add(n, add(V(1),j))),V(1)))), decl([], chain(
                                assign(y, V(0))
                                #assign(nth(y, add(nth(b,add(n,add(V(1),k))), mul(i, n))), V(0)),
                                #assign(nth(y, add(nth(b,add(n,add(V(1),k))), mul(i, n))), add(nth(y, add(nth(b,add(n,add(V(1),k))), mul(i, n))), mul(deref(add(x, add(n, add(nth(x,n), add(V(1), j))))), deref(add(b, add(n, add(nth(b,n), add(V(1), k))))))))
                            )))
                    )))
            )))
    ));
    end;


DefaultCodegen.Ttrace := meth(self, o, y, x, opts)
	local i, j, n;
	i := Ind();
	j := Ind();
	n := var.fresh_t("n", TInt);
	return decl([], chain(
		loopf(i, V(0), sub(n,V(1)), decl([], chain(
			loopf(j, deref(add(x, i)), deref(add(x, add(i, V(1)))), decl([], chain(
				if1(eq(i, deref(add(x,add(n,j)))), assign(y, add(deref(y), deref(add(x, add(n, add(nth(x,n), add(V(1), j))))))))
			)))
		)))
	));
	end;



DefaultCodegen.VStack := meth(self, o, y, x, opts)
	local iA, jA, v, i, j, n;
	iA := var.fresh_t("iA", TPtr(TInt));
	jA := var.fresh_t("jA", TPtr(TInt));
	#val := var.fresh_t("val", TPtr(TInt));
	v := var.fresh_t("v", TPtr(TInt));
	i := Ind();
	j := Ind();
	n := var.fresh_t("n", TInt);
	return decl([iA, jA], chain(
		assign(iA, x),
		assign(n, V(10)),
		assign(jA, add(x, add(n, V(1)))),
		#assign(val, add(IJ), add(n, add(1, nth(iA, n))))
		loopf(i, V(0), n, decl([v], chain(
			loopf(j, deref(add(iA, i)), deref(add(iA, add(i,V(1)))), chain(
				#assign(deref(add(y,i)), V(0))
				assign(deref(add(y,i)), add(deref(add(y,i)), mul(deref(add(jA, add(tcast(TInt, deref(add(iA,n))),j))), deref(add(v, deref(add(jA, j)))))))
				#assign(deref(add(y,i)), deref(add(y,i)), mul(deref(add(jA, add(nth(iA,n), j))), deref(add(v, deref(add(jA, j)))))))
			)))
		))
	));
	end;


DefaultCodegen.Gath := meth(self, o, y, x, opts)
	local i, index_i, index_j;
	i := Ind();
	index_j := o.func._children[2].params[2];
	return decl([], chain(
		loopf(i, nth(x, index_j), nth(x, add(index_j, V(1))), chain(
			assign(nth(y, sub(i, nth(x, index_j))), nth(x, add(opts.symbol[1], add(nth(x,opts.symbol[1]), add(i, V(1))))))
		))
	));
end;

DefaultCodegen.Scat := meth(self, o, y, x, opts) 
	local i, index_i, index_j;
	i := Ind();
	index_j := o.func._children[2].params[2];
	return decl([], chain(
		loopf(i, nth(x, index_j), nth(x, add(index_j, V(1))), chain(
			assign(nth(y,nth(x, add(opts.symbol[1], add(V(1), i)))), nth(x, sub(i, nth(x, index_j))))
		))
	));
end;

#DefaultCodegen.RowVec2 := meth(self, o, y, x, opts)
#	local body, result, sa;
#	sa := var.fresh_t("sa", TArray(TInt, o.element.object.size));
#	result := var.fresh_t("result", TInt);
#	body := assign(y , itr);
#	return decl([result, sa], chain(
#		assign(result, V(0)),
#		o.traversal(sa, body)
#	));
#	end;
#
DefaultCodegen.NewScalarProduct := meth(self, o, y, x, opts) 
	local itr, num_nz;
	itr := o.rv.element.object.get_iterator();
	num_nz := o.rv.element.object.num_nonzeros("sa2");
	#return decl([itr], chain(
	return decl([], chain(
		assign(y, V(0)),
		assign(itr, V(0)),
		loopw(lt(itr, o.rv.element.object.size), chain(
		if1(logic_and(neq(struct_nth(x, "value", itr), V(0)),neq(struct_nth("sa2", "value", itr), V(0))), chain(
			assign(y, add(deref(y), mul(struct_nth(x, "value", itr), struct_nth("sa2", "value", itr)))),
			assign(itr, add(itr, V(1)))))))));	
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
  X := var("X", TPtr(TInt)),
  XType := TPtr(TInt),
  arrayDataModifier := "",
  arrayBufModifier := "",
  Y := var("Y", TPtr(TInt)),
  matrix := TSparse_Matrix(TSparse(TArray(TInt, 5), TSemiring_Arithmetic(TInt)), []),
  YType := TPtr(TInt),
#  isCSR := true,
#  includes := ["<sparse.h>"],
#  #symbol := ["struct_array sa2"]
));

