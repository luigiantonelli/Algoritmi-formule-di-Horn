import scala.collection.mutable.HashMap

class HornGraph(formula: Formula){
	private var size = formula.getVariables.size+2
	var graph: HashMap[Int, List[Edge]] = new HashMap[Int, List[Edge]](size,1);
	private var nodes: HashMap[Int, Node] = new HashMap[Int, Node](size,1);
	private var numpos: Int = 0;
	private var clauses = formula.getClauses.toList.zipWithIndex;
	private var numargs: HashMap[Int, Int] = new HashMap[Int,Int](clauses.size,1);
	private var successors: HashMap[Int, HashMap[Int, List[Edge]]] = new HashMap[Int, HashMap[Int, List[Edge]]](size,1);
	
	def initNumArgs: Unit = {
		println("initNumArgs")
		var i = (E(),0)
		for(i <- 0 until clauses.size){
			this.numargs.addOne(i -> clauses(i)._1.getLiterals.filter(_ < 0).size)
		}
	}
	def updateNumArgs(n: Node):Unit = n match {
		case VarNode(v, _, _) => {
			println("updateNumArgs")
			val clist = clauses.filter(c => c._1.containsLiteral(-v))
			var i = 0;
			for(i <- 0 until clist.size){
				numargs(clist(i)._2) = numargs(clist(i)._2) - 1
			}
		}
		case _ => ()
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
		/*var i = VarNode(0,0)
		var it=this.graph.keysIterator
		while(it.hasNext){
			//println("\n\n map:" + map + "\n\n")
			println("entro initSuccessors")
			println("\n\n graph:" + this.graph + "\n\n")
			//println(this.getOutEdges(VarNode(1,1)))
			var edges = this.graph(it.next()).groupBy(e => e.getLabel)
			println("ok initSuccessors")
			var mapedges = createHashMapfromMap(edges)
			successors.addOne(i -> mapedges)
			println("iterazione per " + i + "ora succ = " + successors+ "\n\n")
		}
		*/
		var i = 0;
		for(i <- this.graph.keySet){
			println("entro initSuccessors")
			println("\n\n graph:" + this.graph + "\n\n")
			var edges = this.graph(i).groupBy(e => e.getLabel)
			println("ok initSuccessors")
			var mapedges = createHashMapfromMap(edges)
			successors.addOne(i -> mapedges)
			println("iterazione per " + nodes(i) + "ora succ = " + successors+ "\n\n")
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
		(0 until size).map(i => nodes(i)).toList
	}
	
	def getNode(index: Int): Node = {
		this.nodes(index)
	}
	
	def edgeSetup(rg: HashMap[Int, List[Edge]]): HashMap[Int, List[Edge]] = {
		var rawgraph = rg
		rawgraph.foreachEntry((k,v) => v.foreach(e => e.resetVisit))
		rawgraph
	}
	
	def nodeSetup(rg: HashMap[Int, List[Edge]]): HashMap[Int, List[Edge]] = {
		var rawgraph = rg
		rawgraph.foreachEntry((k,v) => nodes(k).setMarked(v.filter(e => e.isNotVisited).size))
		rawgraph
	}
	
	def resetVisit(rg: HashMap[Int, List[Edge]]): HashMap[Int, List[Edge]] = {
		var rawgraph = rg
		rawgraph = this.edgeSetup(rawgraph)
		rawgraph = this.nodeSetup(rawgraph)
		rawgraph
	}
	
	def graphSetup(rg: HashMap[Int, List[Edge]], nodes: HashMap[Int, Node]): Unit = {
		var rawgraph = rg
		this.nodes = nodes
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
		var i = VarNode(0, 0, 0)
		for(i <- nodes){
			assign(i.getVariable) = i.getTruthValue
		}
		this.formula.getResult(assign)
	}
	
	def traverse(index: Int):Unit = {
		var n = nodes(index)
		if(!n.isComputed){
			if(n.getTruthValue){
				n.setComputed
				updateNumArgs(n)
			}
			else{
				println("\n\n" + this.successors + "\n\n")
				println(this.successors.keySet)
				var nsucc = this.successors(index)
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
							traverse(e.getTarget.getIndex)
						}
						else if(e.getTarget.getMarked != 0 && n.getMarked == 0){
							traverse(e.getTarget.getIndex)
						}
					}
					if(!n.isComputed && numargs(i) == 0){
						updateNumArgs(n)
						n.setTruthValue(true)
					}
				}
			}
		}
	}
}
