package sam.crf

import scala.collection.mutable.ArrayBuffer
class LogSumProduct(chain : Chain) {
  chain.makeCliqueTree()
  chain.logComputeCliques()
  val clique = chain.startClique
  //clique.print()
  //chain.print()

  val beliefs = ArrayBuffer[Array[Array[Double]]]()
  val messagesForward = ArrayBuffer[Array[Double]]()
  val messagesBackward = ArrayBuffer[Array[Double]]()
  var logZ = 0.0

  def computeMessages() {
    var current : Clique = clique
    var last : Clique = clique
    while(current.next != null) {
      var message = new Array[Double](clique.size)
      for(i <- 0 until clique.size) {
        message(i) = logSumExp((0 until clique.size).map(j => current.log(j,i) + (if(current.prev != null) messagesForward(current.index-1)(j) else 1)).toArray)
      }
      messagesForward.append(message)
      last = current
      current = current.next
    }

    current = last.next
    while(current.prev != null) {
      var message = new Array[Double](clique.size)
      for(i <- 0 until clique.size) {
        message(i) = logSumExp((0 until clique.size).map(j => current.log(i,j) + (if(current.next != null) messagesBackward(chain.cliqueSize-current.index-2)(j) else 1)).toArray)
      }
      messagesBackward.append(message)
      current = current.prev
    }
  }

  def computeBeliefs() {
    var current : Clique = clique
    while(current != null) {
      var belief = Array.ofDim[Double](clique.size,clique.size)
      for(i <- 0 until clique.size) {
        for(j <- 0 until clique.size) {
          var leftMult : Double = if(current.prev != null) messagesForward(current.index-1)(i) else 1
          var rightMult : Double = if(current.next != null) messagesBackward(chain.cliqueSize-current.index-2)(j) else 1
          belief(i)(j) = current.log(i,j) + leftMult + rightMult
        }
      }
      beliefs.append(belief)
      current = current.next
    }
  }

  def inferUpDown() : LogSumProduct = {
    computeMessages()
    //printMessages()
    computeBeliefs()
    logZ = logSumExp(beliefs(0).flatten)
    //printBeliefs()
    //printZ()
    this
  }

  def printMessages() {
    for(m <- 0 until messagesForward.size) println(m + " " + messagesForward(m).mkString(","))
    println("")
    for(m <- 0 until messagesBackward.size) println(m + " " + messagesBackward(m).mkString(","))
    println("")
  }

  def printBeliefs() {
    for(i <- 0 until beliefs.size) {
      println("Belief " + i)
      for(j <- 1 to clique.size) {
        for(k <- 1 to clique.size) {
          println( j + "\t" + k + "\t" + beliefValue(i)(j)(k))
        }
      }
      println("")
    }
  }

  def printlogZ() {
    println(logZ)
  }

  def beliefValue(i : Int)(j : Int)(k : Int) = beliefs(i)(j)(k)

  def apply(i : Int) : Array[Double] = {
    var marginals : ArrayBuffer[Double] = null
    if(i < 1 || (i-1) > beliefs.size) { return Array[Double]() }
    else if( (i-1) == beliefs.size ) {
      var idx = beliefs.size-1
      marginals = ArrayBuffer[Double]()
      for(j <- 0 until clique.size) {
        var sum = 0.0
        for(k <- 0 until clique.size) {
          sum = logSumExp(Array(sum,beliefValue(idx)(k)(j)))
        }
        marginals.append(sum)
      }
    }
    else {
      var idx = if((i-1)==chain.cliqueSize) (chain.cliqueSize-1) else (i-1)
      marginals = ArrayBuffer[Double]()
      for(j <- 0 until clique.size) {
        var sum = 0.0
        for(k <- 0 until clique.size) {
          sum = logSumExp(Array(sum,beliefValue(idx)(j)(k)))
        }
        marginals.append(sum)
      }
    }
    if(marginals.head.isNaN || logZ.isNaN)
      println("NaN")
    marginals.toArray.map(b => math.exp(b - logZ))
  }

  def apply(i : Int, j : Int) : Array[Double] = {
    if(i < 1 || (i-1) > chain.cliqueSize) return Array[Double]()
    if(j-i!=1) return Array[Double]()
    var idx = if((i-1)==chain.cliqueSize) (chain.cliqueSize-1) else (i-1)
    beliefs(idx).flatten.map(b => math.exp(b - logZ))
  }

  def setToMaxMarginal() {
    var current = clique
    var last = clique
    var count = 1
    while(current != null) {
      val marginal = this(count)
      val max = marginal.max
      val maxIndex = marginal.indexOf(max)
      current.transFactor.left.value = maxIndex+1
      count += 1
      last = current
      current = current.next
    }
    val marginal = this(count)
    last.transFactor.right.value = ( (marginal.indexOf(marginal.max)) +1)
  }

  def printMarginals(i : Int) {
    var current = clique
    var last = clique
    var count = 1
    while(current != null) {
      val marginal = this(count)
      println("P(Y" + count + "=" + i + "|x)=" + marginal(i-1))
      count += 1
      last = current
      current = current.next
    }
    val marginal = this(count)
    println("P(Y" + count + "=" + i + "|x)=" + marginal(i-1))
  }

  def logSumExp(xs : Array[Double]) : Double = {
    if (xs.length == 1) return xs(0);
    val max = xs.max
    var sum = 0.0
    for (i <- 0 until xs.length) {
      if (xs(i) != Double.NegativeInfinity)
        sum += math.exp(xs(i) - max)
    }
    max + math.log(sum)
  }
}
