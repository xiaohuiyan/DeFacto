package sam.crf

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

/**
 * Created by anzaroot on 2/13/14.
 */
class WeightsSpec extends FlatSpec with Matchers {
  "Weights" should "store and retrieve weights in the same order" in {
    val featuresDomain = new FeaturesDomain("featuresDomain")
    val labelDomain = new LabelDomain(10)
    val weights = new Weights(featuresDomain, labelDomain)
    println("Weights length:" + weights.getWeights.length)
    val wArray = (0 until weights.getWeights.length).map(_.toDouble).toArray
    weights.setWeights(wArray)
    val gotten = weights.getWeights
    for(i <- 0 until wArray.length) {
      wArray(i) should be (gotten(i))
    }
  }

  "Weights" should "be indexed the same flat or in layered format" in {
      val featuresDomain = new FeaturesDomain("featuresDomain")
      val labelDomain = new LabelDomain(10)
      val weights = new Weights(featuresDomain, labelDomain)
      println("Weights length:" + weights.getWeights.length)
      val wArray = (0 until weights.getWeights.length).map(_.toDouble).toArray
      //Test feature weights
      val rand = new Random()
      val value = rand.nextInt.toDouble
      val klass = 3
      val feature = 2
      val featureValue = 1
      val transition1 = 1
      val transition2 = 3
      println("Index is: " + weights.index(feature, klass, featureValue))
      wArray( weights.index(feature, klass, featureValue) ) = value
      val valueTrans = rand.nextInt.toDouble
      wArray( weights.transIndex(transition1, transition2) ) = valueTrans
      weights.setWeights(wArray)
      println("Value: " + value)
      println("ValueTrans " + valueTrans)

      println(weights(klass, feature, featureValue))
      println(weights(transition1, transition2))

      weights(klass, feature, featureValue) should be (value)
      weights(transition1, transition2) should be (valueTrans)
  }

}