class Edge(source: Node, target: Node, label: Int, var visited: Boolean){
	def isNotVisited: Boolean = {
		this.visited == false
	}
	
	def setVisited: Unit = {
		this.visited = true
	}
	
	def resetVisit: Unit = {
		this.visited = false
	}
	
	def getSource: Node = {
		this.source
	}
	
	def getTarget: Node = {
		this.target
	}
	
	def getLabel: Int = {
		this.label
	}
	
	override def toString: String = {
		this.source.toString + " -> " + this.target.toString 
	}
}
