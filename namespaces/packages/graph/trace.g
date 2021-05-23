Load(graph);
Import(graph);
opts := SpiralDefaults;
opts.useDeref := false;
opts.globalUnrolling := 0;
i := Ind(3);
newtrace := RowVec(fConst(TInt,3,1)) * Gath(fitrStack(i,fTensor(fBase(i), fBase(i)))); 
rt := RandomRuleTree(last, opts);
srt := SumsRuleTree(rt, opts);
c := Collect(srt, @(1,var, e-> e.id <> i.id));
srt2 := SubstTopDown(Copy(srt), @(1, var, e-> var.range <> c[1]), e -> c[1]);
cs := CodeSums(srt2, opts);
PrintCode("trace", cs, opts);


trace := RowVec(fConst(TInt,3,1)) * Gath(fTensor(fBase(i), fBase(i))); 

#srt2 := Rewrite(srt, RulesTrace, opts);
#How to get the loop to show Diag?