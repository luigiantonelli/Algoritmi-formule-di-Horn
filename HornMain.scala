import scala.collection.mutable.HashMap

object HornMain extends App{
	//var f:Formula = new Formula(Set(U(1), C(Set(1,2,-3)), C(Set(-1,2,3,4)), C(Set(-5,-1)), C(Set(-5,-3)), U(4))) (soddisfacibile)
	//var f: Formula = new Formula(Set(C(Set(1,-2,4)), C(Set(-2,3,4)), C(Set(-1,2,-3,-4)), C(Set(2,-3))))//soddisfacibile (fa anche chiamata ricorsiva)

	var f:Formula = Parser.getFormula
	println(f)
	
	
	var res = Performance.profila(Solver.DPLL(f))
	if(res._1._1){
		println("Soddisfacibile (Ha impiegato " + res._2*1E-9 + " sec).")
		println(res._1._2)
	}
	else{
		println("Non soddisfacibile.")
	}
	
	
	var res2 = Performance.profila(Solver.HornSat(f))
	if(res2._1._1){
		println("Soddisfacibile con HornSat (Ha impiegato " + res2._2*1E-9 + " sec).")
		println(res2._1._2)
	}
	else{
		println("Non soddisfacibile con HornSat (Ha impiegato " + res2._2*1E-9 + " sec).")
	}
	
	var res3 = Performance.profila(Solver.greedyHorn(f))
	if(res3._1._1){
		println("Soddisfacibile con greedyHorn (Ha impiegato " + res3._2*1E-9 + " sec).")
		println(res3._1._2)
	}
	else{
		println("Non soddisfacibile con greedyHorn (Ha impiegato " + res3._2*1E-9 + " sec).")
	} 
	
	var res4 = Performance.profila(Solver.graphHorn(f))
	if(res4._1._1){
		println("Soddisfacibile con graphHorn (Ha impiegato " + res4._2*1E-9 + " sec).")
		println(res4._1._2)
	}
	else{
		println("Non soddisfacibile con graphHorn (Ha impiegato " + res4._2*1E-9 + " sec).")
	} 
	
	var res5 = Performance.profila(Solver.RenamableHornSat(f))
	if(res5._1._1){
		println("Soddisfacibile con RenamableHornSat(Ha impiegato " + res5._2*1E-9 + " sec).")
		println(res5._1._2)
	}
	else{
		println("Non soddisfacibile con RenamableHornSat (Ha impiegato " + res5._2*1E-9 + " sec).")
	} 
}
