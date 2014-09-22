package sam.crf

import java.util

import edu.umass.nlp.ml.feats.WeightsManager

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by samanzaroot on 9/19/14.
 */

abstract class Domain[A] {
  val values : ArrayBuffer[A]
}

class FeaturesDomain[F] extends Domain[F] {
  val values = new ArrayBuffer[F]()
  val reverseMap = new mutable.HashMap[F, Int]()
  var _frozen = false

  def freeze(): Unit = {
    _frozen = true
  }

  def unfreeze() : Unit = {
    _frozen = false
  }

  def +=(feature : F): Unit = {
    if(!_frozen) {
      if(!reverseMap.contains(feature)) {
        values += feature
        reverseMap(feature) = values.length - 1
      }
    }
  }

  def featureIndex(feature : F) : Int = {
    reverseMap(feature)
  }

  def featureValue(index : Int) : F = {
    values(index)
  }
  def size = values.size
}

class StringFeaturesDomain[String] extends FeaturesDomain[String] {
  def +=(feature : String, value : String) : Unit = {
    super.+=( (feature + ":+:" + value))
  }
}

class DoubleIndexedFeatureDomain extends FeaturesDomain[String] {

  var features = new ArrayBuffer[String]
  var featuresValues = new ArrayBuffer[ArrayBuffer[String]]()

  var reverseFeatureValuesMap = new ArrayBuffer[mutable.HashMap[String, Int]]()
  var reverseFeatureMap = new mutable.HashMap[String, Int]()

  /*def initialize(file : String): Unit = {
    for(line <- Source.fromFile(file).getLines()) {
      var split = line.split("->")
      features.append((split(0).toInt to split(1).toInt).toArray)
    }
  }*/

  override def +=(feature : String) {
    System.err.println("Tried to to add unindexed feature into indexed feature domain")
    System.exit(1)
  }

  def +=(feature : String, value : String): Unit = {
    if(!_frozen) {
      if(!reverseFeatureMap.contains(feature)) {
        features += feature
        reverseFeatureMap(feature) = features.length-1
        featuresValues(features.length-1) = new ArrayBuffer[String]()
        reverseFeatureValuesMap(features.length-1) = new mutable.HashMap[String,Int]()
      }
      val featureIndex = reverseFeatureMap(feature)
      if(!reverseFeatureValuesMap(featureIndex).contains(value)) {
        featuresValues(featureIndex) += value
        reverseFeatureValuesMap(featureIndex)(value) = featuresValues(featureIndex).length-1
      }
    }
    super.+=(feature+"+:+"+value)
  }


  /*def featureIndex(featureIndex : Int, value : Int) : Int = {
    val start = features(featureIndex).head
    value - start
  }*/
}

class LabelDomain extends Domain[String] {
  val values = new ArrayBuffer[String]()
  val labels = values
  def until = labels.length
  def length = labels.length
  def initialize(till : Int): Unit = {
   values ++= (0 until till).map(_.toString).toArray
  }
}

abstract class Vector extends Seq[Double] {
  def +=(o : Vector): Unit = {
    assert(o.length == this.length, "Vector lengths must match for addition")
    (0 until length).map( i => this.add(i,o(i)) )
  }

  def +=(inc : Double) : Unit = {
    (0 until length).map( i=> this.add(i,inc))
  }

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

  def activeElements : Seq[(Int,Double)]
  def activeLength : Int

}

abstract class Vector1(val dim1 : Int)

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
}

class SparseVector1(dim1 : Int) extends Vector1(dim1) {
  val map = new mutable.HashMap[Int, Double]()

  val posMap = new mutable.HashMap[Int, Int]()
  val indices = new ArrayBuffer[Int]()
  val values = new ArrayBuffer[Double]()

  def length : Int = dim1
  def activeLength : Int = indices.length + map.size

  def apply(i : Int): Double = {
    if(posMap.contains(i)) {
      val pos = posMap(i)
      values(pos)
    } else if(map.contains(i)) {
      map(i)
    } else 0.0
  }

  def add(i : Int, v : Double) : Unit = {
    if(posMap.contains(i)) {
      val pos = posMap(i)
      values(pos) += v
    } else if(map.contains(i)) {
      map(i) += v
    } else map(i) = v
  }

  def mult(i : Int, v : Double) : Unit = {
    if(posMap.contains(i)) {
      val pos = posMap(i)
      values(pos) *= v
    } else if(map.contains(i)) {
      map(i) *= v
    }
  }

  def mergeMap() : Unit = {
    if(map.size > 0) {
      val sortedMap =  map.toSeq.sortBy(_._1)
      var si = 0; var ii = 0
      var lastInc = -1
      var incBy = 0
      for(i <- 0 until indices.length + sortedMap.length) {
        if(indices(si) < sortedMap(ii)._1) {
          si += 1
        } else {
          indices.insert(si, sortedMap(ii)._1)
          values.insert(si, sortedMap(ii)._2)
          if(lastInc >= 0) {
            for(j <- lastInc until si) {
              posMap(indices(j)) += incBy
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
    indices.zip(values)
  }

  def update(i : Int, v : Double) : Unit = {
    assert(i < dim1 && i >= 0, "Index out of bounds on setting sparse vector")
    if(posMap.contains(i)) {
      val pos = posMap(i)
      values(pos) = v
    } else {
      map(i) = v
    }
  }

  def toDense : DenseVector1 = {
    val o = new DenseVector1(dim1)
    for(e <- activeElements) {
      o(e._1) = e._2
    }
    o
  }

  def dot(v : Vector): Double = {
    if(this.activeLength > v.activeLength) {
      v.activeElements.map(e => e._2 * this(e._1)).sum
    } else {
      this.activeElements.map(e => e._2 * v(e._1)).sum
    }
  }


}

class Weights(val model : Model) {
  val factorWeights = new Array[Vector](model.families.size)
  
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
