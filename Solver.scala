import scala.collection.mutable.HashMap

object Solver{
	def graphHorn(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		var f = formula
		if(!f.isHorn){
			println("La formula non è una formula di Horn.")
			(false,Nil)
		}
		else{
			var (graph, falsenode):(HornGraph, Node) = Utils.buildGraph(f)
			println(graph)
			if(graph.getNumPos == 0){
				(true, graph.getResult)
			}
			else{
				graph.traverse(falsenode)
				if(falsenode.getTruthValue){
					(false, Nil)
				}
				else{
					var n = VarNode(0, 0)
					var nodes = graph.getNodes
					for(n <- nodes){
						if(!n.isComputed)
							graph.traverse(n)
					}				
					(true, graph.getResult)
				}
			}
		}
	}
	
	def greedyHorn(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		var f = formula
		if(!f.isHorn){
			println("La formula non è una formula di Horn.")
			(false,Nil)
		}
		else{
			var assignment = Utils.buildAssignment(f.numVariables)
			var pos:Set[Int] = f.getClauses.map(c => c.getLiterals).filter(c => c.size == 1 && c.head > 0).flatten
			var past:Set[Int] = Set()
			while(!pos.isEmpty){
				var lit = pos.head
				pos = pos - lit
				past = past + lit
				assignment(lit) = true
				f = f.removeLiteralFromClauses(-lit)
				var clpos = f.getClauses.map(c => c.getLiterals).filter(c => c.size == 1 && c.head > 0).flatten
				clpos = clpos -- past
				pos = pos ++ clpos
			}			
			if(f.isSatisfiedBy(assignment))
				(true, f.getResult(assignment))
			else
				(false, Nil)
		}
	}
	
	def HornSat(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		var f = formula.toUnit
		var assignment = Utils.buildAssignment(f.numVariables)
		if(!f.isHorn){
			println("La formula non è una formula di Horn.")
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
	
	private def DPLLaux(formula: Formula, assign: HashMap[Int,Boolean]):(Boolean,HashMap[Int,Boolean]) = {
		var f = formula.toUnit
		var assignment = assign
		if(f.isEmpty) (true, assignment)
		else if(f contains E()) (false, assignment)
		else{
			val up = Utils.unitPropagation(f, assignment)	//unit propagation
			f = up._1
			assignment = up._2
			
			val ple = Utils.pureLiteralElimination(f, assignment)	//pure literal elimination
			f = ple._1
			assignment = ple._2
			val literal = f.chooseLiteral	//choose literal
			if(f.isEmpty) (true, assignment)
			else if(f contains E()) (false, assignment)
			else{
				var fp = new Formula(Set(U(literal.abs)) ++ f.getClauses, f.getCorr)
				var fn = new Formula(Set(U(-literal.abs)) ++ f.getClauses, f.getCorr)
				val p = DPLLaux(fp, assignment)
				if(p._1) p
				else{
					DPLLaux(fn, assignment)
				}
			}
		}
	}
	def DPLL(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		val res = DPLLaux(formula, Utils.buildAssignment(formula.numVariables))
		if(res._1) (true, formula.getResult(res._2))
		else (false, Nil)
	}
}


