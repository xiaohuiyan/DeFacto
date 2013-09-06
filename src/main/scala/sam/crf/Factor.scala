package sam.crf

import math._
import scala.collection.mutable.ArrayBuffer
class Factor(val weights : Weights) {}
class ObservationFactor(val observation : Observation, val label : Label, weights : Weights ) extends Factor(weights) {
	observation.factor = this
	label.observationFactor = this
	val index = observation.index
	def apply(klass : Int) : Double = {
		var sum = 0.0
		var count = 0
		for(feature <- observation.features) {
			sum += weights(klass, count, feature)
			count += 1
		}
		exp(sum)
	}
  def log(klass : Int) : Double = {
    var sum = 0.0
    var count = 0
    for(feature <- observation.features) {
      sum += weights(klass, count, feature)
      count += 1
    }
    sum
  }

}
class TransitionFactor(val left : Label, val right : Label, weights : Weights) extends Factor(weights) {
	left.rightFactor = this
	right.leftFactor = this
	val y = left.index
	val yiPlus = right.index 
	def apply(yi : Int, yPlus : Int) : Double = exp(weights(yi, yPlus))
  def log(yi : Int, yPlus : Int) : Double = weights(yi, yPlus)

  def size = weights.labels.until
}
