import scala.collection.mutable.HashMap

object Parser{
	def getFormula: Formula = {
		var clausestoret = Set[Clause]() //insieme delle clausole per creare la formula
		var clauses = Set[Set[String]]() 
		var ok = true
		var containsempty = false
		println("Scrivi le clausole della formula utilizzando ! per il not")
		while(ok){
			println("Inserisci una clausola della formula con i literal separati da uno spazio (' ') oppure 0 per concludere: ")
			var input = scala.io.StdIn.readLine()
			if(input == "0")
				ok = false
			else{
				var lit:List[String] = input.split(' ').toList
				var clause = Set[String]()
				if(lit contains ""){
					ok = false
					containsempty = true
				}
				else{
					var i = ' '
					for(i <- lit){
						clause = clause + i
					}
					clauses = clauses + clause
				}
			}
		}
		if(containsempty){
			println("La formula che hai inserito contiene una clausola vuota quindi è insoddisfacibile")
			new Formula(Set(E()), new HashMap[Int,String](0,1))
		}
		else{
			var variables = clauses.flatten.map(l => if(l.head == '!') l.tail else l)
			var corr = variables.toList.zipWithIndex.map(t => (t._1, t._2+1))
			if(corr == Nil){
				new Formula(Set(E()), new HashMap[Int,String](0,1))
			}
			else{
				var corrtoret = new HashMap[Int,String](variables.size,1)
				var i = 0
				for(i <- 0 until corr.size){
					corrtoret.addOne(corr(i)._2 -> corr(i)._1)
				}
				var clausesInt = clauses.map(c => c.map(l => if(l.head == '!') corr.filter(_._1 == l.tail).map(-_._2).head else corr.filter(_._1 == l).map(_._2).head))
				clausestoret = clausesInt.map(e => if(e.isEmpty) E() else C(e))
				new Formula(clausestoret, corrtoret).toUnit
			}
		}
	}
}
