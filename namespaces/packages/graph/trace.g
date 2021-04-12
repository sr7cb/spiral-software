Load(graph);
Import(graph);
opts := SpiralDefaults;
opts.useDeref := false;
opts.globalUnrolling := 0;
i := Ind(3);
trace := RowVec(fConst(TInt,1,1)) * Gath(fTensor(fBase(i), fBase(i))); #How to get the loop to show Diag?
rt := RandomRuleTree(last, opts);
srt := SumsRuleTree(rt, opts);
cs := CodeSums(srt, opts);
PrintCode("trace", cs, opts);
