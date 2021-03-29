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
kernel := scat * SPLScope(RowVec(FDataOfs(scp,6,V(3))), scp)  * gath;
kernel2 := Rewrite(kernel, RulesSPLScope, opts);
mxm := ISum(i, 3, ISum(j, 3, kernel2));
rt := RandomRuleTree(last, opts);
srt := SumsRuleTree(rt, opts);
cs := CodeSums(srt, opts);
PrintCode("mxm", cs, opts);


#trace := ISumAcc(i, 3, Gath(fTensor(fBase(i), fBase(i)))); #RowVec(FConst(1s)) * Gath(from over there)
#mpt := trace * mxm;

#kernel := scat * SPLScope(RowVec(diagMul(FDataOfs(scp,6,V(3)))* gath), scp);

#scp.dims := (self) >> [6];