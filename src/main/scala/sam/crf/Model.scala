package sam.crf

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by samanzaroot on 9/22/14.
 */

abstract class Family(val dimentions : Int*) {
  val numOfDimentions = dimentions.length
  var weightIndex : Int = 0
}

class Family1[L <: Domain](val dom1 : L) extends Family(dom1.length) {
  val dim1 = dom1.length
}
class Family2[L <: Domain,R <: Domain](val dom1 : L, val dom2 : R) extends Family(dom1.length, dom2.length) {
  val dim1 = dom1.length
  val dim2 = dom2.length
  def factor(value : dom1.valuetype, value2 : dom2.valuetype, model : Model) : DiscreteFactor2[dom1.valuetype,dom2.valuetype] = {
    new DiscreteFactor2[dom1.valuetype, dom1.valuetype](value, value2, this, model)
  }
}
class Family3[L <: Domain,R <: Domain,U <: Domain](val dom1 : L, val dom2 : R, val dom3 : U) extends Family(dom1.length, dom2.length, dom3.length) {
  val dim1 = dom1.length
  val dim2 = dom2.length
  val dim3 = dom3.length
}

abstract class Factor {
  def score : Double
}

abstract class Factor1[L <: Value](val value : Value, val family : Family1, val model : Model) extends Factor {
  def score : Double
}

class DiscreteFactor1[L <: DiscreteValue](value : Value, fam : Family1, mod : Model) extends Factor1(value, fam, mod) {
  def score : Double = {
    value.value.asInstanceOf[Vector] dot mod.weights(fam.weightIndex)
  }
  def proportions : Seq[Double] = {
    mod.weights(fam.weightIndex)
  }
}

abstract class Factor2[L <: Value, R <: Value](val lValue : Value, val rValue : Value, val family : Family2, val model : Model) extends Factor

class DiscreteFactor2[L <: DiscreteValue, R <: DiscreteValue](l : L, r : R, fam : Family2[_,_], mod : Model) extends Factor2(l,r,fam,mod) {
  def score : Double = {
    (l.value.asInstanceOf[Vector] outer r.value.asInstanceOf[Vector]) dot mod.weights(fam.weightIndex)
  }
}

class Example extends ArrayBuffer[LabelValue]()

abstract class Model {
  val families = new mutable.HashSet[Family]()
  val weights : Weights
  def factors(e : Example) : Seq[Factor]
}
