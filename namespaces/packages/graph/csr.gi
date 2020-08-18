Class(SP, SumsBase, BaseMat, rec(
    _short_print := true,
	rChildren := self >> [self.sr, self.list1, self.list2],
	rSetChild := rSetChildFields("sr","list1", "list2"),
	dims := self >> [self.list1.dimensions[2], self.list2.dimensions[2]],

	
	new := (self, sr, list1, list2) >> SPL(WithBases(self,
		rec(dimensions := [list1.dimensions[2], list2.dimensions[2]],
		sr := Checked(IsSemiring(sr), sr),
		list1 := list1,
		list2 := list2))),
	
));

Class(SP2, SumsBase, BaseMat, rec(
    _short_print := true,
	rChildren := self >> [self.list1, self.list2],
	rSetChild := rSetChildFields("list1", "list2"),

	new := (self, list1, list2) >> SPL(WithBases(self,
		rec(
		list1 := list1,
		list2 := list2))),

	intersect := meth(list1, list2)
		local i, x, result;
		x := 1;
		i := 1;
		result := 0;
		while i < Length(list1) and x < Length(list2) do
			if list1[i] < list2[x] then
				i := i + 1;
			elif list2[x] < list1[i] then
				x := x + 1;
			else 
				result := result + 1;
				i := i +1;
				x := x +1;
			fi;
		od;
		return result;
	end,
));

Class(Intersect, BaseOperation, rec(
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

Class(ScalarProd, TaggedNonTerminal, rec(
  abbrevs := [ (n) -> [n] ],
  isReal := self >> true,
  dims := self >> [self.params[1], self.params[1]]
));

NewRulesFor(ScalarProd, rec(
  Intersect := rec(
    info := "set intersection across two vectors",
    maxSize := false,
    applicable := nt -> not nt.hasTags(),
    children := (self, nt) >> let(i := Ind(), n := nt.params[1], sr := TSemiring_Arithmetic(n.t),
              [[ Intersect(sr, [i, V(0)], [V(0), V(0)], [i, V(0)], i, i, n)]]),
    apply := (nt, C, ct) -> let(i := C[1]._children[1].rt.node.d1, n := nt.params[1], sr := TSemiring_Arithmetic(n.t),C[1])
  ),
));

DefaultSumsGen.Intersect := (self, o, opts) >> o;

DefaultSumsGen.SP := (self, o, opts) >> o;

DefaultCodegen.SP := meth(self, o, y, x, opts) 
	local start, iA, jA, e1, istart, jstart, kstart, kend;
	iA := var.fresh_t("iA", TPtr(x.t));
	jA := var.fresh_t("jA", TPtr(x.t));
	start := var.fresh_t("start", TPtr(x.t));
	e1 := var.fresh_t("e1", TPtr(x.t));
	istart := var.fresh_t("istart", TPtr(x.t));
	jstart := var.fresh_t("jstart", TPtr(x.t));
	kstart := var.fresh_t("kstart", TPtr(x.t));
	kend := var.fresh_t("kend", TPtr(x.t));
	return decl([jstart,start, kstart,kend],chain(
	assign(y, o.sr.zero()),
	assign(jstart, start),
	assign(kstart, add(jA, deref(add(iA, deref(istart)))),
	assign(kend, add(jA, deref(add(iA, add(deref(istart),V(1)))))),	
	loopw(logic_and(lt(jstart, e1), lt(kstart,kend)),
	chain(
		if3(lt(deref(jstart), deref(kstart)),
			assign(jstart, add(jstart,V(1))),
			lt(deref(kstart), deref(jstart)),
			assign(kstart, add(kstart, V(1))),
			chain(
				assign(y, o.srt.sum(y, V(1))),
				assign(jstart, add(jstart, V(1))),
				assign(kstart, add(kstart, V(1))))))))));
end;

TCDefaults := CopyFields(SpiralDefaults, rec(
  compileStrategy := GraphIndicesCS,
  X := var("IJ", TPtr(TInt)),
  XType := TPtr(TInt),
  arrayDataModifier := "",
  arrayBufModifier := "",
  Y := var("res", TPtr(TInt)),
  YType := TPtr(TInt),
  isCSR := true,
  includes := ["<include/sparse.h>"],
));

