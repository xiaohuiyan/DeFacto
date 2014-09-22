package sam.crf

import scala.collection.mutable.ArrayBuffer

/**
 * Created by samanzaroot on 9/19/14.
 */
class Inference(chain : Chain) {

    val logTable = new Array[Double](chain.weights.labels.until)
    var logZ = 0.0

    def printlogZ() {
      println(logZ)
    }

    def go() : Inference = {
      val factor = chain.startClique.factors.head.asInstanceOf[ObservationFactor]
      for(i <- 0 until chain.weights.labels.until) {
        logTable(i) = factor.log(i)
      }
      logZ = logSumExp(logTable)
      normalize()
      this
    }

    def normalize() : Unit = {
      for(i <- 0 until chain.weights.labels.until) {
        logTable(i) = math.exp(logTable(i) - logZ)
      }
    }

    def apply(i : Int) : Double = {
      logTable(i)
    }

    def setToMaxMarginal() {
        val max = logTable.max
        val maxIndex = logTable.indexOf(max)
        chain.startClique.factors.head.asInstanceOf[ObservationFactor].label.value = maxIndex
    }

    def printMarginals() {
      for(i <- 0 until logTable.size) {
        println("P(i="+i+"|x) = " + logTable(i))
      }
    }

    def logSumExp(xs : Array[Double]) : Double = {
      if (xs.length == 1) return xs(0);
      val max = xs.max
      var sum = 0.0
      val until = xs.length
      var i = 0
      while(i < until) {
        if (xs(i) != Double.NegativeInfinity)
          sum += math.exp(xs(i) - max)
        i += 1
      }
      max + math.log(sum)
    }
  }