import scala.collection.mutable.HashMap

object Solver{
	def RHProp(ul: Set[Int], f: Formula, perm_val:HashMap[Int,(Boolean,Boolean)],temp_val:HashMap[Int,(Boolean,Boolean)], r: Boolean):
	(Set[Int], HashMap[Int,(Boolean,Boolean)], HashMap[Int,(Boolean,Boolean)])= {
		var unsetlit = ul
		(unsetlit, perm_val, temp_val)
	}
	def RHTempProp(ul: Set[Int], f: Formula, perm_val:HashMap[Int,(Boolean,Boolean)],term_val:HashMap[Int,(Boolean,Boolean)], r: Boolean):
	(Set[Int], HashMap[Int,(Boolean,Boolean)], HashMap[Int,(Boolean,Boolean)]) = {
		var unsetlit = ul
		var renamable = r
		var x = unsetlit.head
		unsetlit = unsetlit - x
		if(!t_val(x)._1)
			RHProp(unsetvar, f, perm_val, temp_val)
		t_val(x)._1 = true
		t_val(x)._2 = true
		var clauses = f.getClauses.filter(c => c containsLiteral (-x))
		var c = E()
		for(c <- clauses){
			var lits = c.getLiterals.filter(l => l != -x)
			var l = 0
			for(l <- lits){
				if(!renamable) (unsetlit, perm_val, temp_val)
				if(temp_val(l)
			}
		}
		
		(unsetlit, perm_val, temp_val)
	}
	def RenamableHornSat(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		var assignment = Utils.buildAssignment(formula.numVariables)
		var up = Utils.unitPropagation(formula, assignment)
		var f = up._1
		assignment = up._2
		if(f contains E())
			(false, Nil)
		var numVar = f.numVariables
		var temp_val = new HashMap[Int,(Boolean,Boolean)](numVar,1)
		var perm_val = new HashMap[Int,(Boolean,Boolean)](numVar,1)
		var i = 0;
		for(i <- f.getLiterals){
			temp_val.addOne(i -> (false,false))
			perm_val.addOne(i -> (false,false))
		}
		//insieme dei letterali che hanno temp_val e perm_val non impostato
		var unsetlit: Set[Int] = f.getLiterals
		var renamable = true
		while(renamable && !unsetlit.isEmpty){
			val rhtp = RHTempProp(unsetlit, f, perm_val, temp_val, renamable)
			unsetlit = rhtp._1
			perm_val = rhtp._2
			temp_val = rhtp._3
			renamable = rhtp._4
		}
		if(!renamable){
			if(f.isBinary){//????????
				println("La formula è Horn-rinominabile dopo la UnitPropagation, ma è insoddisfacibile")
				(false, Nil)
			}
			else{
				println("La formula non è Horn-rinominabile")
				(false, Nil)
			}
		}
		else{
			//ciclo da rifare
			for(i <- perm_val.keySet){
				assignment(i) = perm_val(i)._1
			}
			(true, formula.getResult(assignment))
		}
	}
	
	def graphHorn(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		var f = formula
		if(!f.isHorn){
			println("La formula non è una formula di Horn.")
			(false,Nil)
		}
		else{
			var graph: HornGraph = Utils.buildGraph(f)
			//println(graph)
			if(graph.getNumPos == 0){
				(true, graph.getResult)
			}
			else{
				graph.traverse(0)
				if(graph.getNode(0).getTruthValue){
					(false, Nil)
				}
				else{
					var n = VarNode(0, 0, 0)
					var nodes = graph.getNodes
					for(n <- nodes){
						if(!n.isComputed)
							graph.traverse(n.getIndex)
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


