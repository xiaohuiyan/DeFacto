package sam.crf

import scala.collection.mutable.ArrayBuffer
class Clique(val size : Int) {
	var index : Int = -1
	var factors = List[Factor]()
	def addFactor(factor : Factor) {
		factors = List(factor) ::: factors
	}
	var transFactor : TransitionFactor = null
	var next : Clique = null
	var prev : Clique = null
	
	val table : Array[Double] = new Array[Double](size*size)
	
	def decode(i : Int) : (Int, Int) = ( ( (i%size)+1 ), ( (i/size)+1 ) )
	
	def apply(yi : Int, yPlus : Int) : Double = table( ( (yPlus-1)*size) + (yi-1) )
	
	def compute() {		
		//println("Clique " + index + " id " + this)
		/*println("size: " + factors.size)
		for(factor <- factors) factor match {
			case o:ObservationFactor => println(o)
			case t:TransitionFactor => println(t)
		}*/
		var obsFactors = new Array[ObservationFactor](2)
		for (factor <- factors)	if(factor.isInstanceOf[TransitionFactor]) transFactor = factor.asInstanceOf[TransitionFactor]
		//println("TransFactor: " + transFactor)
		for (factor <- factors) {
			if(factor.isInstanceOf[ObservationFactor]) obsFactors(factor.asInstanceOf[ObservationFactor].index-transFactor.y) = factor.asInstanceOf[ObservationFactor]
		}
		for(i <- 0 until (size*size)) {
			val idx = decode(i)
			table(i) = transFactor(idx._1, idx._2)
			//println("i: " + i)
			//println("idx: " + idx)
			
			if(obsFactors(0) != null) table(i) *= obsFactors(0)(idx._1)
			if(obsFactors(1) != null) table(i) *= obsFactors(1)(idx._2)
		} 
	}
	
	def print() {
		println(index)
		for(i <- 1 to size) {
			for(j <- 1 to size) {
				println(this(i,j))
			}
		}
		println("")
		if(next != null) next.print()
	}
	
}
