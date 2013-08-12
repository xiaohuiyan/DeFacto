package sam.crf

import scala.collection.mutable.ArrayBuffer
class Node {
}

class Observation(val index : Int, val token : String, listFeatures : Array[Int] ) extends Node {
	var factor : ObservationFactor = null
	var features = listFeatures.map(_.toInt)
}

class Label(val index : Int, val target : String) extends Node {
	var value : Int = -1
	var leftFactor : TransitionFactor = null
	var rightFactor : TransitionFactor = null
	var observationFactor : ObservationFactor = null
}
