import scala.collection.mutable.HashMap

class HornGraph(formula: Formula){
	var graph: HashMap[Node, List[Edge]] = new HashMap[Node, List[Edge]](formula.getVariables.size+2,1);
	var numpos: Int = 0;
	var clauses = formula.getClauses.toList.zipWithIndex;
	var numargs: HashMap[Int, Int] = new HashMap[Int,Int](clauses.size,1);
	var successors: HashMap[Node, HashMap[Int, List[Edge]]] = new HashMap[Node, HashMap[Int, List[Edge]]](formula.getVariables.size+2,1);
	
	def initNumArgs: Unit = {
		println("initNumArgs")
		var i = (E(),0)
		for(i <- 0 until clauses.size){
			this.numargs.addOne(i -> clauses(i)._1.getLiterals.filter(_ < 0).size)
		}
	}
	def updateNumArgs(n: Int):Unit = {
		println("updateNumArgs")
		val clist = clauses.filter(c => c._1.containsLiteral(-n))
		var i = 0;
		for(i <- 0 until clist.size){
			numargs(clist(i)._2) = numargs(clist(i)._2) - 1
		}
	}
	private def createHashMapfromMap(map: Map[Int, List[Edge]]):HashMap[Int, List[Edge]] = {
		var i = 0;
		var keys = map.keySet
		var maptoret = new HashMap[Int, List[Edge]](keys.size, 1)
		for(i <- keys){
			maptoret.addOne(i -> map(i))
		}
		maptoret
	}
	def initSuccessors: Unit = {
		var i = VarNode(0,0)
		for(i <- this.graph.keySet){
			//println("\n\n map:" + map + "\n\n")
			println("entro initSuccessors")
			println("\n\n graph:" + this.graph + "\n\n")
			//println(this.getOutEdges(VarNode(1,1)))
			var edges = this.graph(i).groupBy(e => e.getLabel)
			println("ok initSuccessors")
			var mapedges = createHashMapfromMap(edges)
			successors.addOne(i -> mapedges)
			println("iterazione per " + i + "ora succ = " + successors+ "\n\n")
		}
	}
	def setNumPos: Unit = {
		println("setNumPos")
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
	
	def edgeSetup(rg: HashMap[Node, List[Edge]]): HashMap[Node, List[Edge]] = {
		var rawgraph = rg
		rawgraph.foreachEntry((k,v) => v.foreach(e => e.resetVisit))
		rawgraph
	}
	
	def nodeSetup(rg: HashMap[Node, List[Edge]]): HashMap[Node, List[Edge]] = {
		var rawgraph = rg
		rawgraph.foreachEntry((k,v) => k.setMarked(v.filter(e => e.isNotVisited).size))
		rawgraph
	}
	
	def resetVisit(rg: HashMap[Node, List[Edge]]): HashMap[Node, List[Edge]] = {
		var rawgraph = rg
		rawgraph = this.edgeSetup(rawgraph)
		rawgraph = this.nodeSetup(rawgraph)
		rawgraph
	}
	
	def graphSetup(rg: HashMap[Node, List[Edge]]): Unit = {
		var rawgraph = rg
		println("\n\nRawgraph" + rawgraph + "\n\n")
		rawgraph = this.resetVisit(rawgraph)
		println("\n\nRawgraph dopo modifica" + rawgraph + "\n\n")
		this.graph = rawgraph	
		this.initNumArgs
		this.setNumPos
		this.initSuccessors
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
		if(!n.isComputed){
			if(n.getTruthValue){
				n.setComputed
				updateNumArgs(n.getVariable)
			}
			else{
				println("\n\n" + this.successors + "\n\n")
				println(this.successors.keySet)
				var nsucc = this.successors(n)
				println(this.successors.keySet)
				println(nsucc)
				var i = 0;
				for(i <- nsucc.keySet){
					var outedges = nsucc(i)
					
					var e = new Edge(n,n,0,false)
					for(e <- outedges){
						if(e.isNotVisited){
							n.decreaseMarkedValue
							e.setVisited
							traverse(e.getTarget)
						}
						else if(e.getTarget.getMarked != 0 && n.getMarked == 0){
							traverse(e.getTarget)
						}
					}
					if(!n.isComputed && numargs(i) == 0){
						updateNumArgs(n.getVariable)
						n.setTruthValue(true)
					}
				}
			}
		}
	}
}
