import scala.collection.mutable.HashMap

object Utils{
	private def removeClauses(c: List[(Clause, Int)], s: List[(Clause,Int)]) = {
			val cset = c.toSet
			val sset = s.toSet
			(cset -- sset).toList
	}
	def buildGraph(formula: Formula):HornGraph = {
		val variables = formula.getVariables
		var g = new HashMap[Int, List[Edge]](variables.size + 2, 1)
		var nodes = new HashMap[Int, Node](variables.size + 2, 1)
		var i = 0;
		var truenode = TruthNode(true, 0, variables.size+1)
		var falsenode = TruthNode(false, 0, 0)
		nodes.addOne(0 -> falsenode)
		nodes.addOne(variables.size+1 -> truenode)
		g.addOne(0 -> List[Edge]())
		g.addOne(variables.size+1 -> List[Edge]())
		
		for(i <- variables){
			var node = VarNode(i, 0, i)
			nodes.addOne(i -> node)
			g.addOne(i -> List[Edge]())
			
		}
		var clauses = formula.getClauses.toList.zipWithIndex
		var posunitclauses = clauses.filter(c => c._1.isUnit).filter(c => c._1.getLiterals.head > 0)
		clauses = removeClauses(clauses, posunitclauses)
		var c = (E(),0)
		for(c <- posunitclauses){
			var n = nodes(c._1.getLiterals.head)
			n.initTruthValue(true)
			g(c._1.getLiterals.head) = List(new Edge(n, truenode, c._2, false))
		}
		for(c <- clauses){
			if(c._1.isNegative){
				var l = 0
				var falsenodeedges = g(0)
				for(l <- c._1.getLiterals){
					falsenodeedges = (new Edge(falsenode, nodes(l.abs), c._2, false))::falsenodeedges 
				}
				g(0) = falsenodeedges
			}
			else{
				var poslit = c._1.filter(l => l > 0).head
				var l = 0
				var n = nodes(poslit)
				var edges = g(poslit)
				for(l <- c._1.getLiterals){
					if(l < 0){
						edges = (new Edge(n, nodes(l.abs), c._2, false))::edges 
					}
				}
				g(poslit) = edges
			}
		}
		var graph = new HornGraph(formula)
		graph.graphSetup(g, nodes)
		graph
	}
	
	def unitPropagation(formula: Formula, assign:HashMap[Int,Boolean]):(Formula, HashMap[Int,Boolean]) = {
		var f = formula
		var assignment = assign
		while(f.containsUnitClauses){
			val unit = f.getFirstUnitClause
			val lit = unit.getLiterals.head
		    if(lit > 0) assignment(lit) = true
		    else assignment(-lit) = false	    
			f = f.removeClause(unit)
			var clit = f.getClauses.filter(c => c containsLiteral lit)
			f = f.removeClauses(clit)
			f = f.removeLiteralFromClauses(-lit)
			f = f.toUnit
		}
		(f, assignment)		
	}
	def pureLiteralElimination(formula: Formula, assign:HashMap[Int,Boolean]):(Formula, HashMap[Int,Boolean]) = {
		var (p,n) = formula.getPureLiterals
		var pos = p.toList
		var neg = n.toList
		var f = formula
		var assignment = assign
		var i = 0;
		var j = 0;
		for(i <- pos){
			assignment(i) = true
			var clpos = f.getClauses.filter(c => c containsLiteral i)
			f = f.removeClauses(clpos)
		}
		for(j <- neg){
			assignment(-j) = false
			var clneg = f.getClauses.filter(c => c containsLiteral j)
			f = f.removeClauses(clneg)
		}
		(f.toUnit, assignment)		
	}
	def buildAssignment(n: Int):HashMap[Int,Boolean] = {
		var i = 1;
		var assignment = new HashMap[Int, Boolean](n,1)
		while(i <= n){
			assignment.addOne(i -> false)
			i += 1
		}
		assignment
	}
	
	def invertFormula(f: Formula):Formula = {
		new Formula(f.getClauses.map(c => c.map(e => -e)), f.getCorr)
	}
}
