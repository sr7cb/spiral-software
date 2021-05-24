Load(graph);
Import(graph);
opts := SpiralDefaults;
opts.useDeref := false;
opts.globalUnrolling := 0;
t := var.fresh_t("t", TPtr(TReal));
i := Ind(3);
j := Ind(3);
scat := Scat(fTensor(fBase(i), fBase(j))); 
gath := Gath(fStack(fTensor(fBase(i), fId(3)), fTensor(fId(3), fBase(j))));
scp := SPLScope.freshScope();
kernel := scat * SPLScope(RowVec(FDataOfs(scp,6,V(3))), scp) * gath;
mxm := ISum(i, 3, ISum(j, 3, kernel));
k := Ind(3);
trace := RowVec(fConst(TInt,3,1)) * Gath(fitrStack(k,fTensor(fBase(k), fBase(k)))); 
mpt := trace * mxm;
mpt2 := Rewrite(mpt, RulesMG, opts);
mpt3 := Rewrite(mpt2, RulesMR, opts);
mpt4 := Rewrite(mpt3, RulesMR2, opts);
rt := RandomRuleTree(last, opts);
srt := SumsRuleTree(rt, opts);
srt2 := Rewrite(srt, RulesMTSigma, opts);
cs := CodeSums(srt, opts);
PrintCode("mxm", cs, opts);


#kernel2 := Rewrite(kernel, RulesSPLScope, opts);
mxm := ISum(i, 3, ISum(j, 3, kernel2));
#mpt2 := Rewrite(mpt, RulesMxmTrace, opts);
#trace := ISumAcc(i, 3, Gath(fTensor(fBase(i), fBase(i)))); #RowVec(FConst(1s)) * Gath(from over there)
#mpt := trace * mxm;

#kernel := scat * SPLScope(RowVec(diagMul(FDataOfs(scp,6,V(3)))* gath), scp);

#scp.dims := (self) >> [6];

mxm2 := ISum(i, 3,
  ISum(i, 3,
    SPLScope(
      Scat(fTensor(fBase(i), fBase(i))) * 
      RowVec(fCompose(FDataOfs(scp, 3, V(0)), fTensor(fId(3), fBase(i)))) * 
      Gath(fitrStack(k,fTensor(fBase(k), fBase(k)))), 
      scp
    )
  )
);