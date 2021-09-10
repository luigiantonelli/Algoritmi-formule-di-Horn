import scala.collection.mutable.HashMap

object HornMain extends App{
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
	
	var res4 = Performance.profila(Solver.graphHornSat(f))
	if(res4._1._1){
		println("Soddisfacibile con graphHornSat (Ha impiegato " + res4._2*1E-9 + " sec).")
		println(res4._1._2)
	}
	else{
		println("Non soddisfacibile con graphHornSat (Ha impiegato " + res4._2*1E-9 + " sec).")
	} 
	
	var res5 = Performance.profila(Solver2.RenamableHornSat(f))
	if(res5._1._1){
		println("Soddisfacibile con RenamableHornSat(Ha impiegato " + res5._2*1E-9 + " sec).")
		println(res5._1._2)
	}
	else{
		println("Non soddisfacibile con RenamableHornSat (Ha impiegato " + res5._2*1E-9 + " sec).")
	}
	
	val res6 = Performance.profila(Solver2.dualHornSat(f))
	if(res6._1._1){
		println("Soddisfacibile con dualHornSat(Ha impiegato " + res6._2*1E-9 + " sec).")
		println(res6._1._2)
	}
	else{
		println("Non soddisfacibile con dualHornSat (Ha impiegato " + res6._2*1E-9 + " sec).")
	}
	
	val res7 = Performance.profila(Solver2.dualHornSat2(f))
	if(res7._1._1){
		println("Soddisfacibile con dualHornSat2(Ha impiegato " + res7._2*1E-9 + " sec).")
		println(res7._1._2)
	}
	else{
		println("Non soddisfacibile con dualHornSat2 (Ha impiegato " + res7._2*1E-9 + " sec).")
	}
}
