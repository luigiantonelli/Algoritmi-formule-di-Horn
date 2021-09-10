import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

object Solver2{
	def dualHornSat(formula: Formula):(Boolean, List[(String, Boolean)]) = {
		if(!formula.isDualHorn){
			println("La formula non è una formula Dual-Horn.")
			(false,Nil)
		}
		else if(formula.isEmpty) (true, Nil)
		else if(formula contains E()) (false, Nil)
		else{
			var f = formula
			var n = f.numVariables
			var assignment = new HashMap[Int, Boolean](n,1)
			var i = 1;
			while(i <= n){
				assignment.addOne(i -> true)
				i += 1
			}
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
	
	def RHProp(x: Int, ul: scala.collection.Set[Int], f: Formula, p_val:HashMap[Int,(Boolean,Boolean)],t_val:HashMap[Int,(Boolean,Boolean)], r: Boolean):
	(scala.collection.Set[Int], HashMap[Int,(Boolean,Boolean)], HashMap[Int,(Boolean,Boolean)], Boolean)= {
		var unsetlit = ul
		var renamable = r
		var temp_val = t_val
		var perm_val = p_val
		
		var stack = new Stack[Int](p_val.size)
		var i = 0;
		stack.push(x)
		while(!stack.isEmpty){
			var l = stack.pop()
			var clauses = f.getClauses.filter(c => c containsLiteral (-l))
			var c = E()
			for(c <- clauses){
				var lits = c.getLiterals.filter(_ != l)
				var y = 0
				for(y <- lits){
					if(perm_val(y) == (false, true))
						(Set(), perm_val, temp_val, false)
					else if(!perm_val(y)._2){
						perm_val(y) = (true, true)
						perm_val(-y) = (false, true)
						stack.push(y)
					}
				}
			}
		}
		
		(perm_val.keySet.filter(k => perm_val(k)._2 && temp_val(k)._2), perm_val, temp_val, renamable)
	}
	def RHTempProp(x: Int, ul: scala.collection.Set[Int], f: Formula, p_val:HashMap[Int,(Boolean,Boolean)],t_val:HashMap[Int,(Boolean,Boolean)], r: Boolean):
	(scala.collection.Set[Int], HashMap[Int,(Boolean,Boolean)], HashMap[Int,(Boolean,Boolean)], Boolean) = {
		var unsetlit = ul
		var renamable = r
		var temp_val = t_val
		var perm_val = p_val
		unsetlit = unsetlit - x
		if(temp_val(x) == (false, true))
			RHProp(x, unsetlit, f, perm_val, temp_val, renamable)
		temp_val(x) = (true, true)
		temp_val(-x) = (false, true)
		println("\n\n"+ x + "\n\n")
		var clauses = f.getClauses.filter(c => c containsLiteral (-x))
		var c = E()
		println("\n\n" + clauses + "\n\n")
		if(clauses.isEmpty)
			(unsetlit, perm_val, temp_val, renamable)
		else{
			for(c <- clauses){
				var lits = c.getLiterals.filter(_ != -x)
				var l = 0
				for(l <- lits){
					if(!renamable) (unsetlit, perm_val, temp_val, false)
					if((!temp_val(l)._1 || !temp_val(l)._2)&&(!perm_val(l)._1 || !perm_val(l)._2)){
						val rhtp = RHTempProp(l, unsetlit, f, perm_val, temp_val, renamable)
						unsetlit = rhtp._1
						perm_val = rhtp._2
						temp_val = rhtp._3
						renamable = rhtp._4
					}
				}
			}
			(unsetlit, perm_val, temp_val, renamable)
		}
		
	}
	def RenamableHornSat(formula: Formula):(Boolean, List[(String,Boolean)]) = {
		var f = formula
		var assignment = Utils.buildAssignment(formula.numVariables)
		var up = Utils.unitPropagation(formula, assignment)
		f = up._1
		assignment = up._2
		if(f.isEmpty)
			(true, formula.getResult(assignment))
		else if(f contains E())
			(false, Nil)
		else{
			var numVar = f.numVariables
			var temp_val = new HashMap[Int,(Boolean,Boolean)](numVar,1)
			var perm_val = new HashMap[Int,(Boolean,Boolean)](numVar,1)
			var i = 0;
			for(i <- f.getVariables){
				temp_val.addOne(i -> (false,false))
				perm_val.addOne(i -> (false,false))
				temp_val.addOne(-i -> (false,false))
				perm_val.addOne(-i -> (false,false))
			}
			//insieme dei letterali che hanno temp_val e perm_val non impostato
			var unsetlit: scala.collection.Set[Int] = f.getLiterals
			var renamable = true
			while(renamable && !unsetlit.isEmpty){
				val rhtp = RHTempProp(unsetlit.head, unsetlit, f, perm_val, temp_val, renamable)
				unsetlit = rhtp._1
				perm_val = rhtp._2
				temp_val = rhtp._3
				renamable = rhtp._4
			}
			if(!renamable){
				println("La formula non è soddisfacibile oppure non è Horn-rinominabile")
				(false, Nil)
			}
			else{
				for(i <- perm_val.keySet.map(_.abs)){
					if(perm_val(i)._2)
						assignment(i) = perm_val(i)._1
					else
						assignment(i) = temp_val(i)._1						
				}
				if(formula.isSatisfiedBy(assignment))
					(true, formula.getResult(assignment))
				else
					(false, Nil)
			}
		}
	}
}
