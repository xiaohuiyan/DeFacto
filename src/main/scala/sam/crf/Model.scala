package sam.crf

import scala.collection.mutable

/**
 * Created by samanzaroot on 9/22/14.
 */

class Family(val dimentions : Int*) {
  val numOfDimentions = dimentions.length
  val weightIndex : Int = 0
}

class Model {
  val families = new mutable.HashSet[Family]()
}
