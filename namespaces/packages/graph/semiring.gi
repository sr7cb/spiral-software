IsSymInf := x -> IsSymbolic(x) and IsBound(x.isSymInf) and x.isSymInf;

Class(SymInf, Symbolic, rec(
  __call__ := (self, t) >> WithBases(self, rec(
    t := Checked(IsType(t), t),
    operations := rec(Print := x-> When(IsBound(x.print), x.print(), Print(x.__name__)))
  )),
  print := self >> Print(self.__name__, "(", self.t, ")"),
  isSymInf := true,
  isValue := true,
  ev := self >> self,
  computeType := self >> self.t
));

# ==========================================================================
# Semiring
# ==========================================================================

IsSemiring := x -> IsType(x) and IsBound(x.isSemiring) and x.isSemiring;

IsSemiring_Arithmetic := x -> IsSemiring(x) and IsBound(x.isSemiring_Arithmetic) and x.isSemiring_Arithmetic;

IsSemiring_MinPlus := x -> IsSemiring(x) and IsBound(x.isSemiring_MinPlus) and x.isSemiring_MinPlus;

Class(TSemiring, CompositeTyp, rec(
  __call__ := (self, t) >> WithBases(self, rec(
    t := Checked(IsType(t), t),
    operations := TypOps
  )),
  base_t := self >> self.t,
  isSemiring := true,

  print := self >> Print(self.__name__, "(", self.t, ")"),

  check := (self, v) >> Cond(
    UnifyTypes([self.t, InferType(v)]) = self.t,
      v,
    Error("type mismatch: semiring is type ", self.t)
  ),

  rChildren := self >> [self.t],
  rSetChild := rSetChildFields("t"),
  from_rChildren := (self, rch) >> self,

  zero := self >> Error("method on undefined semiring"),
  one := self >> Error("method on undefined semiring"),

  sum := self >> Error("method on undefined semiring"),
  product := self >> Error("method on undefined semiring")
));

Class(TSemiring_Arithmetic, TSemiring, rec(
  zero := self >> self.t.zero(),
  one := self >> self.t.one(),

  sum := (self, v1, v2) >> Cond(
    UnifyTypes([self.t, InferType(v1), InferType(v2)]) = self.t,
      self.t.sum(v1, v2),
    Error("type mismatch: semiring is type ", self.t)
  ),
  product := (self, v1, v2) >> Cond(
    UnifyTypes([self.t, InferType(v1), InferType(v2)]) = self.t,
      self.t.product(v1, v2),
    Error("type mismatch: semiring is type ", self.t)
  ),

  OpenMP_sum := "+",
  OpenMP_product := "*",

  isSemiring_Arithmetic := true
));

Class(TSemiring_MinPlus, TSemiring, rec(
  zero := self >> SymInf(self.t),
  one := self >> self.t.zero(),

  sum := (self, v1, v2) >> Cond(
    IsSymInf(v1) and IsSymInf(v2),
      SymInf(self.t),
    IsSymInf(v1) and UnifyTypes([self.t, v2.t]) = self.t,
      v2,
    IsSymInf(v2) and UnifyTypes([self.t, v1.t]) = self.t,
      v1,
    UnifyTypes([self.t, InferType(v1), InferType(v2)]) = self.t,
      min(v1, v2),
    Error("type mismatch: semiring is type ", self.t)
  ),
  product := (self, v1, v2) >> Cond(
    IsSymInf(v1) or IsSymInf(v2),
      SymInf(self.t),
    UnifyTypes([self.t, InferType(v1), InferType(v2)]) = self.t,
      self.t.sum(v1, v2),
    Error("type mismatch: semiring is type ", self.t)
  ),

  OpenMP_sum := "min",
  OpenMP_product := "+",

  isSemiring_MinPlus := true
));