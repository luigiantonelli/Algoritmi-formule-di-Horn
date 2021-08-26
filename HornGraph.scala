import scala.collection.mutable.HashMap

class HornGraph(var graph: HashMap[Node, List[Edge]], formula: Formula){
	var numpos: Int = 0;
	var clauses = formula.getClauses.toList.zipWithIndex
	var numargs: HashMap[Int, Int] = new HashMap[Int,Int](clauses.size,1);
	var poslit: List[Int] = formula.getPositiveLiterals
	var successors: HashMap[Node, Map[Int, List[Edge]]] = new HashMap[Node, Map[Int, List[Edge]]](poslit.size+2,1);
	
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
	def initSuccessors(map: HashMap[Int, Node], falsenode: Node, truenode: Node): Unit = {
		println("initSuccessors")
		var i = 0;
		for(i <- poslit){
			var node = map(i)
			var edges = this.getOutEdges(node).groupBy(e => e.getLabel)
			successors.addOne(node -> edges)
		}
		successors.addOne(falsenode -> this.getOutEdges(falsenode).groupBy(e => e.getLabel))
		successors.addOne(truenode -> this.getOutEdges(truenode).groupBy(e => e.getLabel))
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
	
	def graphSetup(map: HashMap[Int, Node], falsenode: Node, truenode: Node): Unit = {
		this.initNumArgs
		this.setNumPos
		this.initSuccessors(map, falsenode, truenode)
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
		if(!n.isComputed){
			if(n.getTruthValue){
				n.setComputed
				updateNumArgs(n.getVariable)
			}
			else{
				var nsucc = this.successors(n)
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
