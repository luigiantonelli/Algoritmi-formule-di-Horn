import scala.collection.mutable.HashMap

class HornGraph(formula: Formula){
	private var size = formula.getVariables.size+2
	var graph: HashMap[Int, List[Edge]] = new HashMap[Int, List[Edge]](size,1);
	private var nodes: HashMap[Int, Node] = new HashMap[Int, Node](size,1);
	private var clauses = formula.getClauses.toList.zipWithIndex;
	private var neglist: HashMap[Int, List[Int]] = new HashMap[Int, List[Int]](size-2,1);
	private var numargs: HashMap[Int, Int] = new HashMap[Int,Int](clauses.size,1);
	private var successors: HashMap[Int, HashMap[Int, List[Edge]]] = new HashMap[Int, HashMap[Int, List[Edge]]](size,1);
	
	private def initNegList: Unit = {
		var i = 0;
		for(i <- 1 to size){
			neglist.addOne(i -> List())
		}
		for(i <- 0 until clauses.size){
			val lits = clauses(i)._1.getLiterals.filter(_ < 0)
			var j = 0
			for(j <- lits){
				neglist(j.abs) = clauses(i)._2::neglist(j.abs)
			}
		}
	}
	private def initNumArgs: Unit = {
		var i = (E(),0)
		for(i <- 0 until clauses.size){
			this.numargs.addOne(i -> clauses(i)._1.getLiterals.filter(_ < 0).size)
		}
		initNegList
	}
	private def updateNumArgs(n: Node):Unit = n match {
		case VarNode(v, _, _) => {
			val clist = neglist(n.getVariable)
			var i = 0;
			for(i <- 0 until clist.size){
				numargs(clist(i)) = numargs(clist(i)) - 1
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
	private def initSuccessors: Unit = {
		var i = 0;
		for(i <- this.graph.keySet){
			var edges = this.graph(i).groupBy(e => e.getLabel)
			var mapedges = createHashMapfromMap(edges)
			successors.addOne(i -> mapedges)
		}
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
		rawgraph = this.resetVisit(rawgraph)
		this.graph = rawgraph	
		this.initNumArgs
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
				var nsucc = this.successors(index)
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
