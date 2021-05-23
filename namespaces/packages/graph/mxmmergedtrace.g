Load(graph);
Import(graph);
opts := SpiralDefaults;
opts.useDeref := false;
opts.globalUnrolling := 0;
i := Ind(3);
scat := Scat(fId(1)); 
gath := Gath(fStack(fTensor(fBase(i), fId(3)), fTensor(fId(3), fBase(i))));
scp := SPLScope.freshScope();
kernel := scat * SPLScope(RowVec(FDataOfs(scp,6,V(3))), scp)  * gath;
kernel2 := Rewrite(kernel, RulesSPLScope, opts);
mxm := ISumAcc(i, i.range, kernel2);
rt := RandomRuleTree(last, opts);
srt := SumsRuleTree(rt, opts);
cs := CodeSums(srt, opts);
PrintCode("mxmmergedtrace", cs, opts);


#Has bug where need the scatter to be outside the SPLScope or the trace wont accumulate