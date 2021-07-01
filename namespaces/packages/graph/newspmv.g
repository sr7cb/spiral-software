Load(graph);
Import(graph);
opts := SpiralDefaults;
opts.useDeref := false;
opts.globalUnrolling := 0;
n := var.fresh_t("n", TInt);
opts.symbol := [n];
i := Ind(3);
itr := var.fresh_t("itr", TInt);
sa := ScatAcc(fTensor(fId(3), fBase(i)));
ts := TSparse(TArray(TInt, 5), TSemiring_Arithmetic(TInt));
fdos := FDataSparseOfs(ts, 5, 0);
spa := fdos.var.get_var();
opts.symbol :=opts.symbol::[spa];
diag := Diag(fConst(TInt, 3, fdos.var.get_elem_value(spa, itr)));
g := Gath(fTensor(fId(3), fBase(i))); 
spmv := ISum(i, opts.symbol[1], sa * COND(eq(fdos.var.get_elem_index(spa, itr), i), diag, O(i, i.range)) * g);
rt := RandomRuleTree(last, opts);
srt := SumsRuleTree(rt, opts);
cs := CodeSums(srt, opts);
PrintCode("spmv", cs, opts);

#sa := ScatAcc(fTensor(fId(3), fBase(i))); #Function is wrong should be in 1 variable as we return a vector