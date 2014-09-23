package sam.crf

import scala.collection.mutable

/**
 * Created by samanzaroot on 9/22/14.
 */

abstract class Family(val dimentions : Int*) {
  val numOfDimentions = dimentions.length
  var weightIndex : Int = 0
}

class Family1[L <: Domain](val dim1 : Int) extends Family(dim1)
class Family2[L <: Domain,R <: Domain](val dim1 : Int, val dim2 : Int) extends Family(dim1, dim2)
class Family3[L <: Domain,R <: Domain,U <: Domain](val dim1 : Int, val dim2 : Int, val dim3 : Int) extends Family(dim1, dim2, dim3)

class Model {
  val families = new mutable.HashSet[Family]()
}
