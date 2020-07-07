Class(GathIntoVec, BaseOperation, rec(
  __call__ := (self, sr, var, range) >> SPL(WithBases(self, rec(
    sr := Checked(IsSemiring(sr), sr),
	var := var,
	range := range,
    _children := [range]
  ))),
  dims := self >> [self.var, V(1)],
  from_rChildren := (self, rch) >> CopyFields(self, rec(_children := rch)),
  doNotMarkBB := true,
  print := (self, i, is) >> Print(
        self.name, "(", self.sr, ", ", self.var, ", ", self.range, ")"
  )
));


DefaultSumsGen.GathIntoVec := (self, o, opts) >> o;


DefaultCodegen.GathIntoVec := meth(self, o, y, x, opts)
  local i, x_first, x_last;
  if (IsBound(opts.isCSR) and opts.isCSR) then
    i := var.fresh_t("i", x.t);
    x_first := var.fresh_t("x_first", x.t);
    x_last := var.fresh_t("x_last", x.t);
	return	decl([i, x_first, x_last], chain (
		assign(x_first, add(x, o.var)),
		assign(x_last, add(x, add(o.var, V(1)))),

		loopf(i, deref(x_first), deref(x_last)),
		  chain(	
			assign(deref(add(y,sub(i,x_first))), sub(deref(x_last), deref(x_first))) 
		  )
		)	
	); 
  fi;
end;


