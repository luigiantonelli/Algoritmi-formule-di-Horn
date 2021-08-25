import scala.collection.mutable.HashMap

class HornGraph(var graph: HashMap[Node, List[Edge]], formula: Formula){
	var numpos: Int = 0;
	var clauses = formula.getClauses.toList.zipWithIndex
	var numargs: HashMap[Int, Int] = new HashMap[Int,Int](clauses.size,1);
	var successors: HashMap[Node, HashMap[Int, List[Edge]]] = new HashMap[Node, HashMap[Int, List[Edge]]](formula.numPositiveLiterals,1);
	
	def initNumArgs: Unit = {
		var i = (E(),0)
		for(i <- 0 until clauses.size){
			this.numargs.addOne(i -> clauses(i)._1.getLiterals.filter(_ < 0).size)
		}
	}
	def initSuccessors: Unit = {
		
	}
	def setNumPos: Unit = {
		this.numpos = this.formula.getClauses.filter(c => c.isUnit && c.getLiterals.head > 0).size
	}
	
	def getNumPos: Int = {
		this.numpos
	}
	
	def getNodes: List[Node] = {
		this.graph.keySet.toList
	}
	def getOutEdges(n: Node):List[Edge] = {
		this.graph(n)
	}
	
	def edgeSetup: Unit = {
		this.graph.foreachEntry((k,v) => v.foreach(e => e.resetVisit))
	}
	
	def nodeSetup: Unit = {
		this.graph.foreachEntry((k,v) => k.setMarked(v.filter(e => e.isNotVisited).size))
	}
	
	def resetVisit: Unit = {
		this.edgeSetup
		this.nodeSetup
	}
	
	def graphSetup: Unit = {
		this.setNumPos
		this.initNumArgs
		this.resetVisit
	}
	
	override def toString: String = {
		println(this.formula.getCorr)
		this.graph.map(t => "Archi uscenti da " + t._1 + ": " + t._2.map(e => e.toString).toString + "\n").toString
	}
	
	def getResult: List[(String,Boolean)] = {
		var nodes = this.getNodes.filter(n => n.isVarNode)
		var assign = Utils.buildAssignment(nodes.size)
		var i = VarNode(0, 0)
		for(i <- nodes){
			assign(i.getVariable) = i.getTruthValue
		}
		this.formula.getResult(assign)
	}
	
	def traverse(n: Node):Unit = {
		
	}
}
