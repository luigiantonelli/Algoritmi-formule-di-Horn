sealed abstract class Node(var marked: Int, val index: Int){
	var computed: Boolean = false;
	var truthvalue: Boolean = false;
	
	def isComputed: Boolean = {
		this.computed
	}
	
	def getTruthValue: Boolean = {
		this.truthvalue
	}
	
	def resetComputed: Unit = {
		this.computed = false
	}
	
	def setComputed: Unit = {
		this.computed = true
	}
	
	def setTruthValue(b: Boolean):Unit = {
		this.computed = true
		this.truthvalue = b
	}
	
	def initTruthValue(b: Boolean):Unit = {
		this.truthvalue = b
	}
	
	def isVarNode: Boolean = this match{
		case VarNode(_,_,_) => true
		case _ => false
	}
	
	def getVariable: Int = this match{
		case VarNode(v,_,_) => v
		case _ => 0
	}
	
	def decreaseMarkedValue: Unit = {
		this.marked = this.marked - 1
	}
	
	def setMarked(x: Int): Unit = {
		this.marked = x
	}
	
	def getMarked: Int = {
		this.marked
	}
	
	def getIndex: Int = {
		this.index
	}
	
	def equals(n: Node): Boolean = (this, n) match {
		case (VarNode(v, m, i), VarNode(v2, m2, i2)) => v == v2 && m == m2 && i == i2
		case (TruthNode(v, m, i), TruthNode(v2, m2, i2)) => v == v2 && m == m2 && i == i2
		case _ => false
	}
}

case class VarNode(variable: Int, var marked_value: Int, val index_value: Int) extends Node(marked_value, index_value){
	override def decreaseMarkedValue: Unit = {
		this.marked_value = this.marked - 1
	}
	
	override def setMarked(x: Int): Unit = {
		this.marked_value = x
	}
	
	override def getMarked: Int = {
		this.marked_value
	}
	
	override def getIndex: Int = {
		this.index_value
	}
}
case class TruthNode(value: Boolean, var marked_value: Int, val index_value: Int) extends Node(marked_value, index_value){
	override def decreaseMarkedValue: Unit = {
		this.marked_value = this.marked - 1
	}
	
	override def setMarked(x: Int): Unit = {
		this.marked_value = x
	}
	
	override def getMarked: Int = {
		this.marked_value
	}
	
	override def getIndex: Int = {
		this.index_value
	}
}

