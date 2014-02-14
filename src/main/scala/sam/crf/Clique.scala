package sam.crf

import scala.collection.mutable.ArrayBuffer
class Clique(val size : Int, val i : Int) {
	var index : Int = -1
	var factors = List[Factor]()
	def addFactor(factor : Factor) {
		factors = List(factor) ::: factors
	}
	var transFactor : TransitionFactor = null
	var next : Clique = null
	var prev : Clique = null
	
	val table = Array.ofDim[Double](size,size)

  val logTable = Array.ofDim[Double](size,size)

	def apply(yi : Int, yPlus : Int) : Double = table(yi)(yPlus)
  def log(yi : Int, yPlus : Int) : Double = logTable(yi)(yPlus)

  def trueLog() : Double = {
    val yi = transFactor.left.targetValue - 1
    val yPlus = transFactor.right.targetValue - 1
    logTable(yi)(yPlus)
  }

  def compute() {
		//println("Clique " + index + " id " + this)
		/*println("size: " + factors.size)
		for(factor <- factors) factor match {
			case o:ObservationFactor => println(o)
			case t:TransitionFactor => println(t)
		}*/
		var obsFactors = new Array[ObservationFactor](2)
    for(factor <- factors) {
      factor match {
        case x : TransitionFactor => transFactor = factor.asInstanceOf[TransitionFactor]
        case x : ObservationFactor => obsFactors(factor.asInstanceOf[ObservationFactor].index-transFactor.y) = factor.asInstanceOf[ObservationFactor]
      }
    }
		for(i <- 0 until size; j <- 0 until size) {
			table(i)(j) = transFactor(i, j)
			//println("i: " + i)
			//println("idx: " + idx)
			
			if(obsFactors(0) != null) table(i)(j) *= obsFactors(0)(i)
			if(obsFactors(1) != null) table(i)(j) *= obsFactors(1)(j)
		} 
	}

  def logCompute() {
    var obsFactors = new Array[ObservationFactor](2)
    for(factor <- factors) {
      factor match {
        case x : TransitionFactor => transFactor = factor.asInstanceOf[TransitionFactor]
        case x : ObservationFactor => obsFactors(factor.asInstanceOf[ObservationFactor].index-transFactor.y) = factor.asInstanceOf[ObservationFactor]
      }
    }
    for(i <- 0 until size; j <- 0 until size) {
      logTable(i)(j) = transFactor.log(i, j)
      if(obsFactors(0) != null) logTable(i)(j) += obsFactors(0).log(i)
      if(obsFactors(1) != null) logTable(i)(j) += obsFactors(1).log(j)
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
