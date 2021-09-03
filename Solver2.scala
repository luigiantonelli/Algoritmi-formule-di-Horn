import scala.collection.mutable.HashMap

object Solver2{
	def dualHornSat(formula: Formula):(Boolean, List[(String, Boolean)]) = {
		var f = formula.toUnit
		var n = f.numVariables
		var assignment = new HashMap[Int, Boolean](n,1)
		var i = 1;
		while(i <= n){
			assignment.addOne(i -> true)
			i += 1
		}
		if(!f.isDualHorn){
			println("La formula non è una formula Dual-Horn.")
			(false,Nil)
		}
		else if(f.isEmpty) (true, f.getResult(assignment))
		else if(f contains E()) (false, Nil)
		else{
			val up = Utils.unitPropagation(f, assignment)
			f = up._1
			assignment = up._2
			if(f contains E())
				(false, Nil)
			else
				(true, formula.getResult(assignment))
		}
	}
	
	def dualHornSat2(formula: Formula):(Boolean, List[(String, Boolean)]) = {
		if(!formula.isDualHorn){
			println("La formula non è una formula Dual-Horn.")
			(false,Nil)
		}
		else{
			var f2 = Utils.invertFormula(formula)
			val res = Solver.HornSat(f2)
			//val res = Solver.greedyHorn(f2)
			//val res = Solver.graphHornSat(f2)
			if(!res._1)
				(false, Nil)
			else
				(true, res._2.map(r => (r._1, !r._2)))
		}
	}
}
