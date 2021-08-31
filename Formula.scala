import scala.collection.mutable.HashMap

class Formula(clauses: Set[Clause], corr: HashMap[Int, String]){
	def getClauses: Set[Clause] = {
		this.clauses
	}
	
	def getCorr: HashMap[Int, String] = {
		this.corr
	}
	
	def isEmpty: Boolean = {
		this.clauses.isEmpty
	}
	
	def numVariables: Int = {
		this.clauses.map(c => c.getLiterals).flatten.map(l => l.abs).size
	}
	
	def isHorn: Boolean = {	
		this.clauses.forall(c => c.isHorn)
	}
	
	def isBinary: Boolean = {
		this.clauses.forall(c => c.size == 2)
	}

    override def toString: String = {
		this.clauses.map(c => c.print(this.corr)+" ∧ ").reduce(_+_).dropRight(3)
	}
	
	def contains(c: Clause): Boolean = {
		this.clauses contains c
	}
	
	def containsUnitClauses: Boolean = {
		this.clauses.filter(_.isUnit).size >= 1
	}
	
	def toUnit: Formula = {
		new Formula(this.clauses.map(_.toUnit), this.corr)
	}
	
	def getPureLiterals: (Set[Int], Set[Int]) = {
		val pos = this.clauses.map(c => c.filter(_ > 0)).flatten
		val neg = this.clauses.map(c => c.filter(_ < 0)).flatten
		val purepos = pos.filter(l => !(neg contains (-l)))
		val pureneg = neg.filter(l => !(pos contains (-l)))
		(purepos,pureneg)
	}
	
	def getLiterals: Set[Int] = {
		this.clauses.map(c => c.getLiterals).flatten
	}
	
	def getPositiveLiterals: List[Int] = {
		this.getLiterals.filter(l => l > 0).toList
	}
	
	def getVariables: Set[Int] = {
		this.getLiterals.map(l => l.abs)
	}
	
	def removeClauses(clist: Set[Clause]):Formula = {
		new Formula(this.clauses -- clist, this.corr)
	}
	def removeClause(c: Clause):Formula = {
		new Formula(this.clauses - c, this.corr)
	}
	
	def removeLiteralFromClauses(lit: Int):Formula = {
		new Formula(this.clauses.map(c => c.removeLiteral(lit)), this.corr)
	}
	
	def getFirstUnitClause: Clause = {
		this.clauses.filter(c => c.isUnit).head
	}
	
	def chooseLiteral: Int = {
		val literals = this.clauses.map(c => c.getLiterals).flatten.toList
		if(literals == Nil) 0
		else literals(0)
	}
	
	def getResult(assign: HashMap[Int,Boolean]):List[(String,Boolean)] = {
		assign.map(a => (this.corr(a._1),a._2)).toList
	}
	
	def isSatisfiedBy(assign: HashMap[Int,Boolean]):Boolean = {
		this.clauses.forall(c => c.isSatisfiedBy(assign))
	}
}
