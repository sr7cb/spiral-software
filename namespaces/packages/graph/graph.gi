Class(Dot_X, BaseOperation, rec(
  __call__ := (self, sr, x_n, s1, s2, range) >> SPL(WithBases(self, rec(
    sr := Checked(IsSemiring(sr), sr),
    start1 := s1,
    start2 := s2,
    range := range,
    x_n := x_n,
    _children := [range]
  ))),
  dims := self >> [self.x_n, V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.sr, ", ", self.start1, ", ", self.start2, ", ", self.range, ")"
  )
));

Class(Accum_X, BaseOperation, rec(
  __call__ := (self, sr, x_n, var, st, range, func) >> SPL(WithBases(self, rec(
    sr := Checked(IsSemiring(sr), sr),
    var := var,
    start := st,
    range := range,
    func := self >> self._children[1],
    x_n := x_n,
    _children := [func, range]
  ))),
  dims := self >> [self.x_n, V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.sr, ", ", self.var, ", ", self.start, ", ", self.range, ",\n",
        Blanks(i+is), self.func().print(i+is, is), "\n",
        Blanks(i), ")"
	)
));

Class(Scat_E, BaseOperation, rec(
  __call__ := (self, sr, x_n, v1, v2, range, svar, func) >> SPL(WithBases(self, rec(
    sr := Checked(IsSemiring(sr), sr),
    v1 := v1,
    v2 := v2,
    range := range,
    svar := svar,
    func := self >> self._children[1],
    x_n := x_n,
    _children := [func, range]
  ))),
  dims := self >> [self.x_n, V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.sr, ", ", self.v1, ", ", self.v2, ", ", self.range, ", ", self.svar, ",\n",
        Blanks(i+is), self.func().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(SMP_Scat_E, BaseOperation, rec(
  __call__ := (self, sr, x_n, v1, v2, range, svar, func, nthreads) >> SPL(WithBases(self, rec(
    sr := Checked(IsSemiring(sr), sr),
    v1 := v1,
    v2 := v2,
    range := range,
    svar := svar,
    func := self >> self._children[1],
    x_n := x_n,
    nthreads := nthreads,
    _children := [func, range]
  ))),
  dims := self >> [self.x_n, V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.sr, ", ", self.v1, ", ", self.v2, ", ", self.range, ", ", self.svar, ",\n",
        Blanks(i+is), self.func().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(CheckEdgesUpdate, BaseOperation, rec(
  __call__ := (self, edges, var, func) >> SPL(WithBases(self, rec(
    edges := edges,
    var := var,
    func := self >> self._children[1],
    _children := [func]
  ))),
  dims := self >> [self.edges, V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.edges, ", ", self.var, ",\n",
        Blanks(i+is), self.func().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(PruneEdges, BaseOperation, rec(
  __call__ := (self, idx, range, var, x_n, s, k) >> SPL(WithBases(self, rec(
    idx := idx,
    range := range,
    var := var,
    x_n := x_n,
    s := s,
    k := k,
    _children := [range, k]
  ))),
  dims := self >> [self.x_n, V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.idx, ", ", self.range, ", ", self.var, ", ", self.s, ", ", self.k, ")"
  )
));

Class(Accum, BaseOperation, rec(
  __call__ := (self, sr, var, range_min, range_max, func) >> SPL(WithBases(self, rec(
    sr := Checked(IsSemiring(sr), sr),
    var := var,
    range_min := range_min,
    range_max := range_max,
    func := self >> self._children[1],
    _children := [func, range_min, range_max]
  ))),
  dims := self >> self._children[1].dims(),
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.sr, ", ", self.var, ", ", self.range_min, ", ", self.range_max, ",\n",
        Blanks(i+is), self.func().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(SMP_Accum, BaseOperation, rec(
  __call__ := (self, sr, var, range_min, range_max, func, nthreads) >> SPL(WithBases(self, rec(
    sr := Checked(IsSemiring(sr), sr),
    var := var,
    range_min := range_min,
    range_max := range_max,
    func := self >> self._children[1],
    nthreads := nthreads,
    _children := [func, range_min, range_max]
  ))),
  dims := self >> self._children[1].dims(),
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.sr, ", ", self.var, ", ", self.range_min, ", ", self.range_max, ",\n",
        Blanks(i+is), self.func().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(Assign_Ptr, BaseOperation, rec(
  __call__ := (self, func) >> SPL(WithBases(self, rec(
    func := self >> self._children[1],
    _children := [func]
  ))),
  dims := self >> self._children[1].dims(),
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(\n",
        Blanks(i+is), self.func().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(Scale_Pt, BaseOperation, rec(
  __call__ := (self, scalar, func) >> SPL(WithBases(self, rec(
    scalar := scalar,
    func := self >> self._children[1],
    _children := [func]
  ))),
  dims := self >> self._children[1].dims(),
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.scalar, ",\n",
        Blanks(i+is), self.func().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(IterateIndefinite, BaseOperation, rec(
  __call__ := (self, op1, op2) >> SPL(WithBases(self, rec(
    op1 := self >> self._children[1],
    op2 := self >> self._children[2],
    _children := [op1, op2]
  ))),
  dims := self >> [V(1), V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(\n",
        Blanks(i+is), self.op1().print(i+is, is), "\n",
        Blanks(i+is), self.op2().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(KTrussWrap, BaseOperation, rec(
  __call__ := (self, s, body) >> SPL(WithBases(self, rec(
    s := s,
    body := self >> self._children[1],
    _children := [body]
  ))),
  dims := self >> [V(1), V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.s, "\n",
        Blanks(i+is), self.body().print(i+is, is), "\n",
        Blanks(i), ")"
  )
));

Class(loopw, Command, rec(
  __call__ := (self, cond, cmd) >> WithBases(self, rec(
    cond := toExpArg(cond),
    cmd := Checked(IsCommand(cmd), cmd),
    operations := CmdOps
  )),
  rChildren := self >> [self.cond, self.cmd],
  rSetChild := rSetChildFields("cond", "cmd"),
  print := (self, i, si) >> Print(self.__name__, "(", self.cond, ",\n",
        Blanks(i+si), self.cmd.print(i+si, si), "\n",
        Blanks(i), ")"
  )
));

Class(loopf, Command, rec(
  __call__ := (self, var, range_min, range_max, cmd) >> WithBases(self, rec(
    var := var,
    range_min := range_min,
    range_max := range_max,
    cmd := Checked(IsCommand(cmd), cmd),
    operations := CmdOps
  )),
  rChildren := self >> [self.var, self.range_min, self.range_max, self.cmd],
  rSetChild := rSetChildFields("var", "range_min", "range_max", "cmd"),
  print := (self, i, si) >> Print(self.__name__, "(", self.var, ", ", self.range_min, ", ", self.range_max, ",\n",
        Blanks(i+si), self.cmd.print(i+si, si), "\n",
        Blanks(i), ")"
  )
));

Class(smp_reduct_loopf, Command, rec(
  __call__ := (self, sr, var, range_min, range_max, y, nthreads, cmd) >> WithBases(self, rec(
    sr := sr,
    var := var,
    range_min := range_min,
    range_max := range_max,
    y := y,
    nthreads := nthreads,
    cmd := Checked(IsCommand(cmd), cmd),
    operations := CmdOps
  )),
  rChildren := self >> [self.sr, self.var, self.range_min, self.range_max, self.y, self.nthreads, self.cmd],
  rSetChild := rSetChildFields("sr", "var", "range_min", "range_max", "y", "nthreads", "cmd"),
  print := (self, i, si) >> Print(self.__name__, "(", self.var, ", ", self.range_min, ", ", self.range_max, ", ", self.nthreads, ",\n",
        Blanks(i+si), self.cmd.print(i+si, si), "\n",
        Blanks(i), ")"
  )
));

Class(smp_loopf, Command, rec(
  __call__ := (self, var, range_min, range_max, nthreads, cmd) >> WithBases(self, rec(
    var := var,
    range_min := range_min,
    range_max := range_max,
    nthreads := nthreads,
    cmd := Checked(IsCommand(cmd), cmd),
    operations := CmdOps
  )),
  rChildren := self >> [self.var, self.range_min, self.range_max, self.nthreads, self.cmd],
  rSetChild := rSetChildFields("var", "range_min", "range_max", "nthreads", "cmd"),
  print := (self, i, si) >> Print(self.__name__, "(", self.var, ", ", self.range_min, ", ", self.range_max, ", ", self.nthreads, ",\n",
        Blanks(i+si), self.cmd.print(i+si, si), "\n",
        Blanks(i), ")"
  )
));

Class(if1, Command, rec(
  __call__ := (self, if_cond, if_cmd) >> WithBases(self, rec(
    if_cond := toExpArg(if_cond),
    if_cmd := Checked(IsCommand(if_cmd), if_cmd),
    operations := CmdOps
  )),
  rChildren := self >> [self.if_cond, self.if_cmd],
  rSetChild := rSetChildFields("if_cond", "if_cmd"),
  print := (self, i, si) >> Print(self.__name__, "(", self.if_cond, ",\n",
        Blanks(i+si), self.if_cmd.print(i+si, si), "\n",
        Blanks(i), ")"
  )
));

Class(if2, Command, rec(
  __call__ := (self, if_cond, if_cmd, ei_cond, ei_cmd, else_cmd) >> WithBases(self, rec(
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

Class(if3, Command, rec(
  __call__ := (self, if_cond, if_cmd, ei_cond, ei_cmd, else_cmd) >> WithBases(self, rec(
    if_cond := toExpArg(if_cond),
    if_cmd := Checked(IsCommand(if_cmd), if_cmd),
    ei_cond := toExpArg(ei_cond),
    ei_cmd := Checked(IsCommand(ei_cmd), ei_cmd),
    else_cmd := Checked(IsCommand(else_cmd), else_cmd),
    operations := CmdOps
  )),
  rChildren := self >> [self.if_cond, self.if_cmd, self.ei_cond, self.ei_cmd, self.else_cmd],
  rSetChild := rSetChildFields("if_cond", "if_cmd", "ei_cond", "ei_cmd", "else_cmd"),
  print := (self, i, si) >> Print(self.__name__, "(", self.if_cond, ",\n",
        Blanks(i+si), self.if_cmd.print(i+si, si), ",\n",
        Blanks(i+si), self.ei_cond, ",\n",
        Blanks(i+si), self.ei_cmd.print(i+si, si), ",\n",
        Blanks(i+si), self.else_cmd.print(i+si, si), "\n",
        Blanks(i), ")"
  )
));

Class(break_ic, Command, rec(
  __call__ := (self) >> WithBases(self, rec(
    operations := CmdOps
  )),
  rChildren := self >> [],
  rSetChild := rSetChildFields(),
  print := (self, i, si) >> Print(self.__name__, "()")
));

CUnparser.loopw := (self,o,i,is) >> Print(Blanks(i),
    "while (", self(o.cond,i,is), ") {\n",
    self(o.cmd,i+is,is), Blanks(i), "}\n");

CUnparser.loopf := (self,o,i,is) >> Print(Blanks(i),
    "for (int ", o.var, " = ", self(o.range_min,i,is), "; ", o.var, " < ", self(o.range_max,i,is), "; ", o.var, "++) {\n",
    self(o.cmd,i+is,is), Blanks(i), "}\n");

CUnparser.smp_reduct_loopf := (self,o,i,is) >> Print(Blanks(i),
    "#pragma omp parallel for schedule(dynamic) reduction(", o.sr.OpenMP_sum, ":", o.y, ") num_threads(", o.nthreads, ")\n",
    Blanks(i), "for (int ", o.var, " = ", self(o.range_min,i,is), "; ", o.var, " < ", self(o.range_max,i,is), "; ", o.var, "++) {\n",
    self(o.cmd,i+is,is), Blanks(i), "}\n");

CUnparser.smp_loopf := (self,o,i,is) >> Print(Blanks(i),
    "#pragma omp parallel for schedule(dynamic) num_threads(", o.nthreads, ")\n",
    Blanks(i), "for (int ", o.var, " = ", self(o.range_min,i,is), "; ", o.var, " < ", self(o.range_max,i,is), "; ", o.var, "++) {\n",
    self(o.cmd,i+is,is), Blanks(i), "}\n");

CUnparser.if1 := (self,o,i,is) >> Print(Blanks(i),
    "if (", self(o.if_cond,i,is), ") {\n", self(o.if_cmd,i+is,is), Blanks(i), "}\n");

CUnparser.if2 := (self,o,i,is) >> Print(Blanks(i),
    "if (", self(o.if_cond,i,is), ") {\n", self(o.if_cmd,i+is,is), Blanks(i), "}",
    " else {\n", self(o.else_cmd,i+is,is), Blanks(i), "}\n");

CUnparser.if3 := (self,o,i,is) >> Print(Blanks(i),
    "if (", self(o.if_cond,i,is), ") {\n", self(o.if_cmd,i+is,is), Blanks(i), "}",
    " else if (", self(o.ei_cond,i,is), ") {\n", self(o.ei_cmd,i+is,is), Blanks(i), "}",
    " else {\n", self(o.else_cmd,i+is,is), Blanks(i), "}\n");

CUnparser.break_ic := (self,o,i,is) >> Print(Blanks(i),
    "break;\n");

Class(VMV_X, TaggedNonTerminal, rec(
  __call__ := (self, sr, s1, sm, s2, d1, d2, x_n) >> WithBases(self, rec(
    sr := Checked(IsSemiring(sr), sr),
    start1 := s1,
    startm := sm,
    start2 := s2,
    d1 := d1,
    d2 := d2,
    x_n := x_n,
    params := []
  )),
  isReal := self >> true,
  dims := self >> [self.x_n, V(1)]
));

Class(TriangleCount, TaggedNonTerminal, rec(
  abbrevs := [ (n) -> [n] ],
  isReal := self >> true,
  dims := self >> [self.params[1], self.params[1]]
));

Class(KTruss, TaggedNonTerminal, rec(
  abbrevs := [ (n, m, k) -> [n, m, k] ],
  isReal := self >> true,
  dims := self >> [self.params[1], self.params[1]]
));

Class(TriangleCountEdges, TaggedNonTerminal, rec(
  abbrevs := [ (n, m, s) -> [n, m, s] ],
  isReal := self >> true,
  dims := self >> [self.params[1], self.params[1]]
));

Class(KTrussPrune, TaggedNonTerminal, rec(
  abbrevs := [ (n, m, k, s) -> [n, m, k, s] ],
  isReal := self >> true,
  dims := self >> [self.params[1], self.params[1]]
));

NewRulesFor(VMV_X, rec(
  Accum_X_Dot := rec(
    info := "Selective accumulation of dot products <Mi,v2> over existing elements in v1",
    maxSize := false,
    applicable := (self, t) >> true,
    apply := (nt, C, ct) -> let(i := Ind(), sr := nt.sr, x_n := nt.x_n,
              Accum_X(sr, x_n, i, nt.start1, nt.d1, Dot_X(sr, x_n, [i, nt.startm[2]], nt.start2, nt.d2)))
  )
));

NewRulesFor(TriangleCount, rec(
  Accum_VMV_FLAME1 := rec(
    info := "FLAME Approach #1",
    maxSize := false,
    applicable := nt -> not nt.hasTags(),
    children := (self, nt) >> let(i := Ind(), n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              [[ VMV_X(sr, [i, V(0)], [V(0), V(0)], [i, V(0)], i, i, n) ]]),
    apply := (nt, C, ct) -> let(i := C[1]._children[1].rt.node.d1, n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              Assign_Ptr(Scale_Pt(V(0.5), Accum(sr, i, V(1), n, C[1]))))
  ),
  Accum_VMV_FLAME2 := rec(
    info := "FLAME Approach #2",
    maxSize := false,
    applicable := nt -> not nt.hasTags(),
    children := (self, nt) >> let(i := Ind(), n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              [[ VMV_X(sr, [i, V(0)], [V(0), i+1], [i, i+1], i, n-i-1, n) ]]),
    apply := (nt, C, ct) -> let(i := C[1]._children[1].rt.node.d1, n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              Assign_Ptr(Accum(sr, i, V(1), n-1, C[1])))
  ),
  Accum_VMV_FLAME4 := rec(
    info := "FLAME Approach #4",
    maxSize := false,
    applicable := nt -> not nt.hasTags(),
    children := (self, nt) >> let(i := Ind(), n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              [[ VMV_X(sr, [i-1, i], [i, i], [i-1, i], n-i, n-i, n) ]]),
    apply := (nt, C, ct) -> let(i := C[1]._children[1].rt.node.startm[1], n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              Assign_Ptr(Scale_Pt(V(0.5), Accum(sr, i, V(1), n, C[1]))))
  ),
  Accum_VMV_FLAME2_SMP := rec(
    info := "FLAME Approach #2",
    maxSize := false,
    applicable := nt -> nt.hasTags(),
    children := (self, nt) >> let(i := Ind(), n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              [[ VMV_X(sr, [i, V(0)], [V(0), i+1], [i, i+1], i, n-i-1, n) ]]),
    apply := (nt, C, ct) -> let(i := C[1]._children[1].rt.node.d1, n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              Assign_Ptr(SMP_Accum(sr, i, V(1), n-1, C[1], nt.firstTag().params[1])))
  )
));

NewRulesFor(KTruss, rec(
  TC_Support_Prune := rec(
    info := "Triangle Count for Support, then Prune. Iterate",
    maxSize := false,
    applicable := nt -> not nt.hasTags(),
    children := (self, nt) >> let(n := nt.params[1], m := nt.params[2], k := nt.params[3], s := var.fresh_t("s", TArray(TInt, nt.params[2])),
              [[ TriangleCountEdges(n, m, s), KTrussPrune(n, m, k, s) ]]),
    apply := (nt, C, ct) -> Assign_Ptr(Scale_Pt(V(0.5), IterateIndefinite(C[1], C[2])))
  ),
  TC_Support_Prune_SMP := rec(
    info := "Triangle Count for Support, then Prune. Iterate",
    maxSize := false,
    applicable := nt -> nt.hasTags(),
    children := (self, nt) >> let(n := nt.params[1], m := nt.params[2], k := nt.params[3], s := var.fresh_t("s", TArray(TInt, nt.params[2])),
              [[ TriangleCountEdges(n, m, s).withTags(nt.tags), KTrussPrune(n, m, k, s) ]]),
    apply := (nt, C, ct) -> Assign_Ptr(Scale_Pt(V(0.5), IterateIndefinite(C[1], C[2])))
  )
));

NewRulesFor(TriangleCountEdges, rec(
  Endpoint_Row_Dot := rec(
    info := "Dot products of rows corresponding to endpoints of each edge",
    maxSize := false,
    applicable := nt -> not nt.hasTags(),
    apply := (nt, C, ct) -> let(v1 := var.fresh_t("v", TInt), v2 := var.fresh_t("v", TInt), n := nt.params[1], m := nt.params[2], s := nt.params[3], sr := TSemiring_Arithmetic(n.t),
              Scat_E(sr, n, v1, v2, m, s, Dot_X(sr, n, [v1, 1], [v2, 1], n)))
  ),
  Endpoint_Row_Dot_SMP := rec(
    info := "Dot products of rows corresponding to endpoints of each edge",
    maxSize := false,
    applicable := nt -> nt.hasTags(),
    apply := (nt, C, ct) -> let(v1 := var.fresh_t("v", TInt), v2 := var.fresh_t("v", TInt), n := nt.params[1], m := nt.params[2], s := nt.params[3], sr := TSemiring_Arithmetic(n.t),
              SMP_Scat_E(sr, n, v1, v2, m, s, Dot_X(sr, n, [v1, 1], [v2, 1], n), nt.firstTag().params[1]))
  )
));

NewRulesFor(KTrussPrune, rec(
  Check_Support_Prune := rec(
    info := "Eliminate edges without enough support and break if nothing changes",
    maxSize := false,
    applicable := nt -> not nt.hasTags(),
    apply := (nt, C, ct) -> let(i := Ind(), v := var.fresh_t("v", TInt), n := nt.params[1], m := nt.params[2], k := nt.params[3], s := nt.params[4],
              CheckEdgesUpdate(m, v, PruneEdges(i, m, v, n, s, k)))
  )
));

DefaultSumsGen.Accum := (self, o, opts) >> Accum(o.sr, o.var, o.range_min, o.range_max, self(o.func(), opts));

DefaultSumsGen.Accum_X := (self, o, opts) >> Accum_X(o.sr, o.x_n, o.var, o.start, o.range, self(o.func(), opts));

DefaultSumsGen.Scat_E := (self, o, opts) >> Scat_E(o.sr, o.x_n, o.v1, o.v2, o.range, o.svar, self(o.func(), opts));

DefaultSumsGen.SMP_Scat_E := (self, o, opts) >> SMP_Scat_E(o.sr, o.x_n, o.v1, o.v2, o.range, o.svar, self(o.func(), opts), o.nthreads);

DefaultSumsGen.Dot_X := (self, o, opts) >> o;

DefaultSumsGen.CheckEdgesUpdate := (self, o, opts) >> CheckEdgesUpdate(o.edges, o.var, self(o.func(), opts));

DefaultSumsGen.PruneEdges := (self, o, opts) >> o;

DefaultSumsGen.SMP_Accum := (self, o, opts) >> SMP_Accum(o.sr, o.var, o.range_min, o.range_max, self(o.func(), opts), o.nthreads);

DefaultSumsGen.Assign_Ptr := (self, o, opts) >> Assign_Ptr(self(o.func(), opts));

DefaultSumsGen.Scale_Pt := (self, o, opts) >> Scale_Pt(o.scalar, self(o.func(), opts));

DefaultSumsGen.IterateIndefinite := (self, o, opts) >> IterateIndefinite(self(o.op1(), opts), self(o.op2(), opts));

DefaultCodegen.Dot_X := meth(self, o, y, x, opts)
  local j1, j1m, j2, j2m;
  if (IsBound(opts.isCSR) and opts.isCSR) then
    j1 := var.fresh_t("j1", x.t);
    j1m := var.fresh_t("j1m", x.t);
    j2 := var.fresh_t("j2", x.t);
    j2m := var.fresh_t("j2m", x.t);
    return decl([j1, j1m, j2, j2m], chain(
      assign(y, o.sr.zero()),
      assign(j1, add(x, o.x_n, V(1), nth(x, o.start1[1]))),
      assign(j1m, add(x, o.x_n, V(1), nth(x, add(o.start1[1], V(1))))),
      assign(j2, add(x, o.x_n, V(1), nth(x, o.start2[1]))),
      assign(j2m, add(x, o.x_n, V(1), nth(x, add(o.start2[1], V(1))))),
      loopw(logic_and(lt(j1, j1m), lt(deref(j1), o.start1[2])),
        assign(j1, add(j1, V(1)))),
      loopw(logic_and(lt(j2, j2m), lt(deref(j2), o.start2[2])),
        assign(j2, add(j2, V(1)))),
      loopw(logic_and(logic_and(lt(j1, j1m), lt(j2, j2m)), logic_and(lt(deref(j1), add(o.start1[2], o.range)), lt(deref(j2), add(o.start2[2], o.range)))),
        if3(lt(deref(j1), deref(j2)),
          assign(j1, add(j1, V(1))),
        lt(deref(j2), deref(j1)),
          assign(j2, add(j2, V(1))),
        chain(
          assign(y, o.sr.sum(y, V(1))),
          assign(j1, add(j1, V(1))),
          assign(j2, add(j2, V(1))))))));
  fi;
end;

DefaultCodegen.Accum_X := meth(self, o, y, x, opts)
  local j, jm, t;
  t := var.fresh_t("t", y.t);
  if (IsBound(opts.isCSR) and opts.isCSR) then
    j := var.fresh_t("j", x.t);
    jm := var.fresh_t("jm", x.t);
    return decl([j, jm], chain(
      assign(y, o.sr.zero()),
      assign(j, add(x, o.x_n, V(1), nth(x, o.start[1]))),
      assign(jm, add(x, o.x_n, V(1), nth(x, add(o.start[1], V(1))))),
      loopw(logic_and(lt(j, jm), lt(deref(j), o.start[2])),
        assign(j, add(j, V(1)))),
      loopw(logic_and(lt(j, jm), lt(deref(j), add(o.start[2], o.range))), decl([o.var, t], chain(
        assign(o.var, deref(j)),
        self(o.func(), t, x, opts),
        assign(y, o.sr.sum(y, t)),
        assign(j, add(j, V(1))))))));
  fi;
end;

DefaultCodegen.Scat_E := meth(self, o, y, x, opts)
  local t, i;
  t := var.fresh_t("t", y.t);
  i := Ind();
  if (IsBound(opts.isCSR) and opts.isCSR) then
    return decl([o.v1, o.svar], chain(
      assign(o.v1, V(0)),
      loopf(i, V(0), o.range, decl([t, o.v2], chain(
        loopw(eq(nth(x, add(o.v1, V(1))), i),
          assign(o.v1, add(o.v1, V(1)))),
        assign(o.v2, nth(x, add(o.x_n, V(1), i))),
        self(o.func(), t, x, opts),
        assign(nth(o.svar, i), t))))));
  fi;
end;

DefaultCodegen.SMP_Scat_E := meth(self, o, y, x, opts)
  local t, i, j;
  t := var.fresh_t("t", y.t);
  i := Ind();
  j := Ind();
  if (IsBound(opts.isCSR) and opts.isCSR) then
    return decl([o.v1, o.svar],
      smp_loopf(i, V(0), o.x_n, o.nthreads,
        loopf(j, nth(x, i), nth(x, add(i, V(1))), decl([o.v1, o.v2, t], chain(
          assign(o.v1, i),
          assign(o.v2, nth(x, add(o.x_n, V(1), j))),
          self(o.func(), t, x, opts),
          assign(nth(o.svar, j), t))))));
  fi;
end;

DefaultCodegen.CheckEdgesUpdate := meth(self, o, y, x, opts)
  return decl([o.var], chain(
    assign(o.var, V(0)),
    assign(y, o.edges),
    self(o.func(), y, x, opts),
    if1(eq(o.edges, o.var),
      break_ic()),
    assign(o.edges, o.var),
    assign(y, o.var)));
end;

DefaultCodegen.PruneEdges := meth(self, o, y, x, opts)
  local t;
  t := var.fresh_t("t", y.t);
  if (IsBound(opts.isCSR) and opts.isCSR) then
    return decl([t], chain(
      assign(t, V(0)),
      loopf(o.idx, V(0), o.range, chain(
        loopw(eq(nth(x, add(t, V(1))), o.idx), chain(
          assign(t, add(t, V(1))),
          assign(nth(x, t), o.var))),
        if1(geq(nth(o.s, o.idx), sub(o.k, V(2))), chain(
          assign(nth(x, add(o.x_n, V(1), o.var)), nth(x, add(o.x_n, V(1), o.idx))),
          assign(o.var, add(o.var, V(1))))))),
      assign(nth(x, add(t, V(1))), o.var)));
  fi;
end;

DefaultCodegen.Accum := meth(self, o, y, x, opts)
  local t;
  t := var.fresh_t("t", y.t);
  return chain(
    assign(y, o.sr.zero()),
    loopf(o.var, o.range_min, o.range_max, decl([t], chain(
      self(o.func(), t, x, opts),
      assign(y, o.sr.sum(y, t))))));
end;

DefaultCodegen.SMP_Accum := meth(self, o, y, x, opts)
  local t;
  t := var.fresh_t("t", y.t);
  return chain(
    assign(y, o.sr.zero()),
    smp_reduct_loopf(o.sr, o.var, o.range_min, o.range_max, y, o.nthreads, decl([t], chain(
      self(o.func(), t, x, opts),
      assign(y, o.sr.sum(y, t))))));
end;

DefaultCodegen.Scale_Pt := meth(self, o, y, x, opts)
  return chain(
    self(o.func(), y, x, opts),
    assign(y, mul(o.scalar, y)));
end;

DefaultCodegen.Assign_Ptr := meth(self, o, y, x, opts)
  local t;
  t := var.fresh_t("t", y.t.t);
  return decl([t], chain(
    self(o._children[1], t, x, opts),
    assign(deref(y), t)));
end;

DefaultCodegen.IterateIndefinite := meth(self, o, y, x, opts)
  return loopw(V(1), chain(
    self(o.op1(), y, x, opts),
    self(o.op2(), y, x, opts)));
end;

GraphIndicesCS := [
    c -> Compile.pullDataDeclsRefs(c),
    c -> Compile.fastScalarize(c),
    c -> UnrollCode(c),
    c -> FlattenCode(c),
    c -> UntangleChain(c),
    #(c, opts) -> CopyPropagate.initial(c, opts),
    #(c, opts) -> HashConsts(c, opts),
    #c -> MarkDefUse(c),
    #(c, opts) -> BinSplit(c, opts),
    #c -> MarkDefUse(c),
    #CopyPropagate, # does CSE
    c -> Compile.declareVars(c)
];

TCDefaults := CopyFields(SpiralDefaults, rec(
  compileStrategy := GraphIndicesCS,
  X := var("IJ", TPtr(TInt)),
  XType := TPtr(TInt),
  arrayDataModifier := "",
  arrayBufModifier := "",
  Y := var("res", TPtr(TInt)),
  YType := TPtr(TInt),
  isCSR := true,
));