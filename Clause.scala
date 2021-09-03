import scala.collection.mutable.HashMap

sealed abstract class Clause{
	def isEmpty: Boolean = this match {
		case E() => true
		case _ => false
	}
	
	def isUnit: Boolean = this match {
		case U(l) => true
		case _ => false
	}
	
	def size: Int = this match {
		case E() => 0
		case U(l) => 1
		case C(v) => v.size
	}
	
	def toUnit: Clause = this match {
		case C(v) if(v.size == 1) => U(v.head)
		case _ => this
	}
	
	def print(corr: HashMap[Int, String]): String = this match {
		case E() => "()"
		case U(l) => if(l > 0) "("+corr(l)+")" else "(¬"+corr(-l)+")"
		case C(v) => "(" + v.map(e => ((if(e > 0) corr(e) else "¬"+corr(-e))+" ∨ ")).reduce(_+_).dropRight(3) + ")"
	}
	
	def isHorn: Boolean = this match {
		case E() => true
		case U(l) => true
		case C(v) => v.count(_ > 0) <= 1
	}
	
	def isDualHorn: Boolean = this match {
		case E() => true
		case U(l) => true
		case C(v) => v.count(_ < 0) <= 1
	}
	
	def containsVariable(x: Int):Boolean = this match {
		case E() => false
		case U(l) => x == l.abs
		case C(v) => {
			val variables = v.map(_.abs)
			variables contains x
		}
	}
	
	def map(f: Int => Int): Clause = this match{
		case E() => E()
		case U(l) => U(f(l))
		case C(v) => C(v.map(e => f(e)))
	}
	
	def filter(f: Int => Boolean): Set[Int] = this match{
		case E() => Set()
		case U(l) => if(f(l)) Set(l) else Set()
		case C(v) => v.filter(f(_))
	}
	
	def getLiterals:Set[Int] = this match{
		case E() => Set()
		case U(l) => Set(l)
		case C(v) => v
	}
	
	def removeLiteral(lit: Int):Clause = this match{
		case E() => E()
		case U(l) => if(l == lit) E() else this
		case C(v) => C(v - lit)
	}
	
	def containsLiteral(lit: Int):Boolean = this match{
		case E() => false
		case U(l) => l == lit
		case C(v) => v contains lit
	}
	
	def isSatisfiedBy(assign: HashMap[Int,Boolean]):Boolean = this match{
		case E() => false
		case U(l) => if(assign(l.abs)) l > 0 else l < 0 
		case C(v) => v.map(e => if(assign(e.abs)) e > 0 else e < 0).count(_ == true) >= 1
	}
	
	def isEqualTo(c: Clause): Boolean = (this, c) match {
		case (E(), E()) => true
		case (U(l), U(l2)) => l == l2
		case (C(v), C(v2)) => v == v2
		case _ => false
	}
	
	def isNegative: Boolean = this match {
		case E() => true
		case U(l) => l < 0
		case C(v) => v.forall(_ < 0)
	}
}

case class E() extends Clause				//clausola vuota
case class U(l: Int) extends Clause			//clausola unitaria
case class C(v: Set[Int]) extends Clause	//clausola composta da più letterali

