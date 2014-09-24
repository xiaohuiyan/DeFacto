package sam.crf

import scala.collection.mutable.ArrayBuffer

/**
 * Created by samanzaroot on 9/23/14.
 */
class BP(val model : Model) {
  class BPFactor1(val factor : Factor1) {
    val message = new DenseVector1(factor.family.dim1)
  }

  class BPFactor2(val factor : Factor2) {
    val leftMessage = new DenseVector1(factor.family.dim1)
    val rightMessage = new DenseVector1(factor.family.dim2)
  }

  var logZ : Double = 0.0

}
