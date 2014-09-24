package sam.crf

import java.util
import scala.reflect.runtime.universe._
import edu.umass.nlp.ml.feats.WeightsManager

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by samanzaroot on 9/19/14.
 */
abstract class Vector extends Seq[Double] {
  def +=(o : Vector): Unit = {
    assert(o.length == this.length, "Vector lengths must match for addition")
    (0 until length).map( i => this.add(i,o(i)) )
  }

  def +=(inc : Double) : Unit = {
    (0 until length).map( i=> this.add(i,inc))
  }
  def apply(i : Int) : Double
  def update(i : Int, d : Double) : Unit
  def add(i : Int, v : Double) : Unit
  def mult(i : Int, v : Double) : Unit

  def dot(o : Vector) : Double = {
    assert(o.length == this.length, "Vector length must match for dot product")
    (0 until length).map( i => this(i)*o(i)).sum
  }

  def *=(o : Vector) : Unit = {
    assert(o.length == this.length, "Vector lengths must match for addition")
    (0 until length).map( i => this.mult(i,o(i)))
  }

  def *=(inc : Double) : Unit = {
    (0 until length).map( i=> this.mult(i, inc))
  }

  def outer(o : Vector) : Vector

  def activeElements : Seq[(Int,Double)]
  def activeLength : Int

  def logSumExp : Double
}

abstract class Vector1(var dim1 : Int) extends Vector
abstract class Vector2(val dim1 : Int, val dim2 : Int) extends Vector
abstract class Vector3(val dim1 : Int, val dim2 : Int, val dim3 : Int) extends Vector

class DenseVector1(dim1 : Int) extends Vector1(dim1) {
  val elements = new Array[Double](dim1)

  def length : Int = elements.length
  def activeLength : Int = elements.length

  def apply(i : Int) = elements(i)

  def add(i : Int, v : Double) : Unit = {
    elements(i) += v
  }

  def mult(i : Int, v : Double) : Unit = {
    elements(i) *= v
  }

  def update(i : Int, v : Double) : Unit = {
    elements(i) = v
  }

  def iterator : Iterator[Double] = {
    elements.toIterator
  }

  def outer(o : Vector) : Vector = {
    val prodLength = o.length*this.length
    val prod = new DenseVector1(prodLength)
    for(i <- 0 until this.length; j <- 0 until this.length) {
      val index = i*this.length + j
      prod.update(index, this(i)*o(j))
    }
    prod
  }

  def toSparse : SparseVector1 = {
    val o = new SparseVector1(dim1)
    for(i <- 0 until length) {
      if(this(i) != 0.0) o(i) = this(i)
    }
    o
  }

  def activeElements : Seq[(Int,Double)] = {
    (0 until length).zip(elements)
  }
  def logSumExp : Double = {
    if (this.length == 1) return this(0);
    val max = elements.max
    var sum = 0.0
    val until = elements.length
    var i = 0
    while(i < until) {
      if (elements(i) != Double.NegativeInfinity)
        sum += math.exp(elements(i) - max)
      i += 1
    }
    max + math.log(sum)
  }
}

class DenseVector2(dim1 : Int, dim2 : Int) extends Vector2(dim1, dim2) {
  val elements = new Array[Double](dim1*dim2)

  def length : Int = elements.length
  def activeLength : Int = elements.length

  def apply(i : Int) = elements(i)

  def indexOf(i : Int, j : Int) = i * dim1 + j

  def apply(i : Int, j : Int) = elements(indexOf(i,j))

  def add(i : Int, v : Double) : Unit = {
    elements(i) += v
  }

  def add(i : Int, j : Int, v : Double) : Unit = {
    elements(indexOf(i,j)) += v
  }

  def mult(i : Int, v : Double) : Unit = {
    elements(indexOf(i)) *= v
  }

  def mult(i : Int, j : Int, v : Double) : Unit = {
    elements(indexOf(i,j)) *= v
  }

  def update(i : Int, v : Double) : Unit = {
    elements(i) = v
  }

  def update(i : Int, j : Int, v : Double) : Unit = {
    elements(indexOf(i,j)) = v
  }

  def iterator : Iterator[Double] = {
    elements.toIterator
  }

  def outer(o : Vector) : Vector = {
    val prodLength = o.length*this.length
    val prod = new DenseVector1(prodLength)
    for(i <- 0 until this.length; j <- 0 until this.length) {
      val index = i*this.length + j
      prod.update(index, this(i)*o(j))
    }
    prod
  }

  def toSparse : SparseVector1 = {
    val o = new SparseVector1(dim1)
    for(i <- 0 until length) {
      if(this(i) != 0.0) o(i) = this(i)
    }
    o
  }

  def activeElements : Seq[(Int,Double)] = {
    (0 until length).zip(elements)
  }

  def logSumExp : Double = {
    if (this.length == 1) return this(0);
    val max = elements.max
    var sum = 0.0
    val until = elements.length
    var i = 0
    while(i < until) {
      if (elements(i) != Double.NegativeInfinity)
        sum += math.exp(elements(i) - max)
      i += 1
    }
    max + math.log(sum)
  }
}

class DenseVector3(dim1 : Int, dim2 : Int, dim3 : Int) extends Vector3(dim1, dim2, dim3) {
  val elements = new Array[Double](dim1*dim2*dim3)

  def length : Int = elements.length
  def activeLength : Int = elements.length

  def apply(i : Int) = elements(i)

  def indexOf(i : Int, j : Int, k : Int) = (i * dim1 * dim2) + (j * dim2) + k

  def apply(i : Int, j : Int, k : Int) = elements(indexOf(i,j,k))

  def add(i : Int, v : Double) : Unit = {
    elements(i) += v
  }

  def add(i : Int, j : Int, k : Int, v : Double) : Unit = {
    elements(indexOf(i,j,k)) += v
  }

  def mult(i : Int, v : Double) : Unit = {
    elements(indexOf(i)) *= v
  }

  def mult(i : Int, j : Int, k : Int, v : Double) : Unit = {
    elements(indexOf(i,j,k)) *= v
  }

  def update(i : Int, v : Double) : Unit = {
    elements(i) = v
  }

  def update(i : Int, j : Int, k : Int, v : Double) : Unit = {
    elements(indexOf(i,j,k)) = v
  }

  def iterator : Iterator[Double] = {
    elements.toIterator
  }

  def toSparse : SparseVector1 = {
    val o = new SparseVector1(dim1)
    for(i <- 0 until length) {
      if(this(i) != 0.0) o(i) = this(i)
    }
    o
  }

  def outer(o : Vector) : Vector = {
    val prodLength = o.length*this.length
    val prod = new DenseVector1(prodLength)
    for(i <- 0 until this.length; j <- 0 until this.length) {
      val index = i*this.length + j
      prod.update(index, this(i)*o(j))
    }
    prod
  }

  def activeElements : Seq[(Int,Double)] = {
    (0 until length).zip(elements)
  }

  def logSumExp : Double = {
    if (this.length == 1) return this(0);
    val max = elements.max
    var sum = 0.0
    val until = elements.length
    var i = 0
    while(i < until) {
      if (elements(i) != Double.NegativeInfinity)
        sum += math.exp(elements(i) - max)
      i += 1
    }
    max + math.log(sum)
  }
}

class OneHotVector(dim1 : Int) extends Vector1(dim1) {
  var index : Int = 0
  var value : Double = 1.0
  val length = dim1
  def mult(i : Int, v : Double) : Unit = {
    assert(i==index, "Mult on 1 hot must be on indexed element")
  }
  def add(i : Int, v : Double) : Unit = {
    assert(i==index, "Add on 1 hot must be on indexed element")
  }
  val activeLength = 1
  def apply(i : Int) : Double = {
    if(i==index) value else 0.0
  }

  def update(i : Int, d : Double) : Unit = {
    assert(i==index, "Update on 1 hot must be on indexed element")
    value = d
  }

  def activeElements : Seq[(Int,Double)] = Seq((index,value))

  def iterator : Iterator[Double] = {
    Array(value).toIterator
  }
  override def dot(o : Vector) : Double = {
    assert(o.length == this.length, "Vector length must match for dot product")
    o(index)*value
  }
  def outer(o : Vector) : Vector = {
    val prodLength = o.length*this.length
    o match {
      case v : SparseVector1 => {
          val prod = new SparseVector1(prodLength)
          for(e <- v.activeElements) {
            val index = this.index*this.length + e._1
            prod.update(index, this.value*e._2)
          }
        prod
      }
      case v : _ => {
        val prod = new DenseVector1(prodLength)
        for(e <- v.activeElements) {
          val index = this.index*this.length + e._1
          prod.update(index, this.value*e._2)
        }
        prod
      }
    }
 }
  def logSumExp : Double = {
    value
  }
}

class SparseVector1(dim : Int) extends Vector1(dim) {
  val tempMap = new mutable.HashMap[Int, Double]()

  val posMap = new mutable.HashMap[Int, Int]()
  val indexes = new ArrayBuffer[Int]()
  val values = new ArrayBuffer[Double]()

  def length : Int = dim1
  def activeLength : Int = indexes.length + tempMap.size

  def apply(i : Int): Double = {
    if(posMap.contains(i)) {
      val pos = posMap(i)
      values(pos)
    } else if(tempMap.contains(i)) {
      tempMap(i)
    } else 0.0
  }

  def add(i : Int, v : Double) : Unit = {
    if(i > length-1) {
      dim1 = i+1
    }
    if(posMap.contains(i)) {
      val pos = posMap(i)
      values(pos) += v
    } else if(tempMap.contains(i)) {
      tempMap(i) += v
    } else tempMap(i) = v
  }

  def iterator : Iterator[Double] = {
    mergeMap()
    values.toIterator
  }

  def mult(i : Int, v : Double) : Unit = {
    if(posMap.contains(i)) {
      val pos = posMap(i)
      values(pos) *= v
    } else if(tempMap.contains(i)) {
      tempMap(i) *= v
    }
  }

  def mergeMap() : Unit = {
    if(tempMap.size > 0) {
      val sortedMap =  tempMap.toSeq.sortBy(_._1)
      var si = 0; var ii = 0
      var lastInc = -1
      var incBy = 0
      for(i <- 0 until indexes.length + sortedMap.length) {
        if(indexes(si) < sortedMap(ii)._1) {
          si += 1
        } else {
          indexes.insert(si, sortedMap(ii)._1)
          values.insert(si, sortedMap(ii)._2)
          if(lastInc >= 0) {
            for(j <- lastInc until si) {
              posMap(indexes(j)) += incBy
            }
          }
          lastInc = si+1
          incBy += 1
          ii += 1
        }
      }
    }
  }

  def activeElements : Seq[(Int,Double)] = {
    mergeMap()
    indexes.zip(values)
  }

  def update(i : Int, v : Double) : Unit = {
    assert(i >= 0, "Index out of bounds on setting sparse vector")
    if(i > length-1) {
      dim1 = i+1
    }
    if(posMap.contains(i)) {
      val pos = posMap(i)
      values(pos) = v
    } else {
      tempMap(i) = v
    }
  }

  def toDense : DenseVector1 = {
    val o = new DenseVector1(dim1)
    for(e <- activeElements) {
      o(e._1) = e._2
    }
    o
  }

  override def dot(v : Vector): Double = {
    if(this.activeLength > v.activeLength) {
      v.activeElements.map(e => e._2 * this(e._1)).sum
    } else {
      this.activeElements.map(e => e._2 * v(e._1)).sum
    }
  }

  def outer(o : Vector) : Vector = {
    val prodLength = o.length*this.length
    val prod = new SparseVector1(prodLength)
    for(te <- this.activeElements) {
      for(e <- o.activeElements) {
        val index = te._1*this.length + e._1
        prod.update(index, te._2*e._2)
      }
    }
    prod
  }

  def logSumExp : Double = {
    val elements = activeElements.map(_._2)
    if (this.length == 1) return this(0);
    val max = elements.max
    var sum = 0.0
    val until = elements.length
    var i = 0
    while(i < until) {
      if (elements(i) != Double.NegativeInfinity)
        sum += math.exp(elements(i) - max)
      i += 1
    }
    max + math.log(sum)
  }
}

class SparseBinaryVector1(dim : Int) extends SparseVector1(dim) {
  override def update(i : Int, v : Double) : Unit = {
    assert(v==1.0 || v==0.0, "vector value must be 1.0 or 0.0")
    super.update(i,v)
  }
  override def add(i : Int, v : Double) {
    assert(v==1.0, "Can only add 1.0 to binary vector")
  }
  override def mult(i : Int, v : Double): Unit = {
    assert(v==1.0, "Can only mult by 1.0 to binary vector")
  }
  def update(i : Int) : Unit = {
    super.update(i,1.0)
  }
}

class Weights(val model : Model) {
  val factorWeights = new Array[Vector](model.families.size)
  var activeWeights = 0
  for(fam <- model.families) fam match {
    case f : Family1 => {
      factorWeights(activeWeights) = new DenseVector1(f.dim1)
      f.weightIndex = activeWeights
      activeWeights += 1
    }
    case f : Family2 => {
      factorWeights(activeWeights) = new DenseVector2(f.dim1, f.dim2)
      f.weightIndex = activeWeights
      activeWeights += 1
    }
    case f : Family3 => {
      factorWeights(activeWeights) = new DenseVector3(f.dim1, f.dim2, f.dim3)
      f.weightIndex = activeWeights
      activeWeights += 1
    }
  }
  def apply(i : Int) = factorWeights(i)
}

/*class WeightsOld(val features : FeaturesDomain, val labels : LabelDomain) {
  val obsWeights = new Array[Array[Array[Double]]](features.size)
  var count = 0

  for(feature <- features.features) {
    obsWeights(count) = Array.ofDim[Double](feature.size,labels.until)
    count += 1
  }

  val transWeights = Array.ofDim[Double](labels.until,labels.until)

  val dimension = obsWeights.map(_.length * labels.until).sum + transWeights.length * transWeights.length
  val obsWeightsSize = obsWeights.map(_.length * labels.until).sum

  def setClassWeights(wts : Array[Double]) {
    //Set the weights from the wts vector here.
    var count = 0
    for(f <-  0 until features.features.size) {
      for(v <- 0 until features.features(f).size ) {
        for(k <- 0 until labels.until) {
          obsWeights(f)(v)(k) = wts(count)
          count += 1
        }
      }
    }
  }

  def setWeights(wts : Array[Double]) {
    //Set the weights from the wts vector here.
    var count = 0
    for(f <-  0 until features.features.size) {
      for(v <- 0 until features.features(f).size ) {
        for(k <- 0 until labels.until) {
          obsWeights(f)(v)(k) = wts(count)
          count += 1
        }
      }
    }
    for(i <- 0 until transWeights.length; j <- 0 until transWeights(0).length) {
      transWeights(i)(j) = wts(count)
      count += 1
    }
  }

  def getWeights : Array[Double] = flatObs ++ flatTrans

  def getClassWeights : Array[Double] = flatObs

  def flatTrans : Array[Double] = transWeights.flatten

  def flatObs : Array[Double] = obsWeights.flatten.flatten

  def apply(klass : Int, feature : Int, value : Int) : Double = {
    obsWeights(feature)(value-features.features(feature)(0))(klass)
  }


  def index(feature : Int, klass : Int, value : Int) : Int = {
    val featureCount = obsWeights.take(feature).map(_.length).sum * labels.until
    featureCount + value*labels.until + klass
  }

  def transIndex(klass1 : Int, klass2 : Int) : Int = {
    obsWeightsSize + labels.until*klass1 + klass2
  }

  def apply(y1 : Int, yPlus : Int) : Double = {
    transWeights(y1)(yPlus)
  }

  def print() {
    for(f <- obsWeights) {
      for(i <- f) {
        println(i)
      }
      println("")
    }
    println("Transition:")
    for(tw <- transWeights) {
      for(t <- tw) {
        println(t)
      }
    }
    println("")
  }

  def printWithName() {
    var to = 0
    var count = 0
    for(f <- obsWeights) {
      println("Feature: " + count)
      var count2 = 0
      for(i <- f) {
        if(count2 % labels.until == 0) println("Label: " + count2/labels.until)
        println("Feature value: " + count2 % labels.until)
        println(i)
        count2 += 1
        to += 1
      }
      println("")
      count += 1
    }
    println("Transition:")
    for(tw <- transWeights) {
      for(t <- tw) {
        println(t)
        to += 1
      }
    }
    println("Total: " + to )
    println("")
  }

}*/
