Load(graph);
Import(graph);
opts := SpiralDefaults;
opts.useDeref := false;
opts.globalUnrolling := 0;
t := var.fresh_t("t", TPtr(TReal));
i := Ind(INF());
j := Ind(INF());
scat := Scat(fTensor(fBase(i), fBase(j))); 
gath := Gath(fStack(fTensor(fBase(i), fId(INF())), fTensor(fId(INF()), fBase(j))));
scp := SPLScope.freshScope();
kernel := scat * SPLScope(RowVec(diagMul(FDataOfs(scp,INF(),INF()))) * gath, scp);
mxm := ISum(i, INF(), ISum(j, INF(), kernel));
rt := RandomRuleTree(last, opts);
srt := SumsRuleTree(rt, opts);
cs := CodeSums(srt, opts);
PrintCode("mxm", cs, opts);