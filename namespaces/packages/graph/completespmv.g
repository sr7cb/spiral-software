Load(graph);
Import(graph);
opts := SpiralDefaults;
opts.useDeref := false;
opts.globalUnrolling := 0;
n := var.fresh_t("n", TInt);
opts.symbol := [n];
i := Ind(3);
ts := TSparse(TArray(TInt, 3), TSemiring_Arithmetic(TInt));
fdataofs := FDataOfs(ts, 3, 0);
md := MakeDiag(fdataofs);
eye := I(i);
sid := SUM(eye, md);
matmul := MatMul(3,3);
tti := TTensorI(Reduce("Col", 3, TInt), 3, AVec, AVec);
spmv := tti * matmul * sid;
spmv2 := Rewrite(spmv, RulesIDiag, opts);
spmv3 := Rewrite(spmv2, RulesMatMul, opts);
rt := RandomRuleTree(last, opts);
srt := SumsRuleTree(rt, opts);
srt2 := Rewrite(srt, RulesSigSPMV1, opts);
srt3 := Rewrite(Copy(srt2), RulesSigSPMV2, opts);
cs := CodeSums(last, opts);
PrintCode("spmv", cs, opts);

#sa := ScatAcc(fTensor(fId(3), fBase(i))); #Function is wrong should be in 1 variable as we return a vector