sealed abstract class Node(var marked: Int){
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
		case VarNode(_,_) => true
		case _ => false
	}
	
	def getVariable: Int = this match{
		case VarNode(v,_) => v
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
}

case class VarNode(variable: Int, var marked_value: Int) extends Node(marked_value){
	override def decreaseMarkedValue: Unit = {
		this.marked_value = this.marked - 1
	}
	
	override def setMarked(x: Int): Unit = {
		this.marked_value = x
	}
	
	override def getMarked: Int = {
		this.marked_value
	}
}
case class TruthNode(value: Boolean, var marked_value: Int) extends Node(marked_value){
	override def decreaseMarkedValue: Unit = {
		this.marked_value = this.marked - 1
	}
	
	override def setMarked(x: Int): Unit = {
		this.marked_value = x
	}
	
	override def getMarked: Int = {
		this.marked_value
	}
}

