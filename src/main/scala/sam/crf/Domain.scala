package sam.crf

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

/**
 * Created by samanzaroot on 9/23/14.
 */
abstract class Domain[A](implicit val ev: TypeTag[A]) {
  val values : ArrayBuffer[A]
  val length : Int
  val domainClass : Class[_] = ev.getClass
  type inner = A
  type valuetype = Value
}

class IntDomain(from : Int, to : Int) extends Domain[Int] {
  val values = ArrayBuffer(from until to)
  val length = values.length
}

class RealDomain(from : Int, to : Int) extends Domain[Double] {
  val values = ArrayBuffer()
  val length = 0
}

abstract class DiscreteDomain[A] extends Domain[A] {
  override type inner = Vector
}

class FeaturesDomain[F] extends DiscreteDomain[F] {
  override type valuetype = FeatureValue
  val values = new ArrayBuffer[F]()
  def length = values.length
  val reverseMap = new mutable.HashMap[F, Int]()
  var _frozen = false
  override type inner = SparseBinaryVector1

  def apply(i : Int) : F = {
    values(i)
  }

  def apply(f : F) : Int = {
    reverseMap(f)
  }

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

class StringFeaturesDomain[String] extends FeaturesDomain[String] {
  def +=(feature : String, value : String) : Unit = {
    val s = feature + ":+:" + value
    super.+=(s)
  }
}

class LabelDomain extends DiscreteDomain[String] {
  override type valuetype = LabelValue
  val values = new ArrayBuffer[String]()
  val labels = values
  def until = labels.length
  def length = labels.length
  def initialize(till : Int): Unit = {
    values ++= (0 until till).map(_.toString).toArray
  }
  override type inner = OneHotVector
}
