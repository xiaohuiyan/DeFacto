package sam.crf

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by samanzaroot on 9/19/14.
 */
class Classifier(domainFile : String, labelDomainSize : Int, ff : (String=>Array[Int]))  {
  val featuresDomain = new FeaturesDomain(domainFile)
  val labelDomain = new LabelDomain(labelDomainSize)
  var datasetSize = 0

  //TODO These weights are sequence model weights, need to abstract weights and allow for sparse weight representation
  val weights = new Weights(featuresDomain, labelDomain)


  //For classifier it is easier to have training set in one file
  def loadExamples(file : String): ArrayBuffer[Chain] = {
    //In a classifier each chain is of length 1
    //TODO refactor such that chain links are independent of chain concept (for other MRFS)
    val examples : ArrayBuffer[Chain] = new ArrayBuffer[Chain]()
    val source = Source.fromFile(file)
    for(line <- source.getLines()) {
      examples += new Chain(weights, ff).loadClassifierChain(line)
    }
    examples
  }

  def evaluate(examples : Array[Chain]) : Unit = {
    var countCorrect = 0
    var countTrue = 0
    var countPredicted = 0
    for(example <- examples) {
      new Inference(example).go().setToMaxMarginal()
      countTrue += 1
      countPredicted += 1
      val label = example.head.observationFactors.head.label
      if(label.value == label.targetValue) countCorrect += 1
    }
    println("Accuracy: " + countCorrect.toDouble/countTrue)
  }

  def test(file : String) {
    val examples = loadExamples(file)
    datasetSize = examples.length
    for(example <- examples) {
      val inf = new Inference(example)
      inf.go()
      inf.setToMaxMarginal()
      inf.printMarginals()
      println("")
    }
  }

  def train(trainExamples : ArrayBuffer[Chain], testExamples : ArrayBuffer[Chain]) : Unit = {
    datasetSize = trainExamples.length
    val trainer = new ClassLBFGSTrainer(this, trainExamples.toArray, testExamples.toArray) with L2Regularization
    trainer.optimize()
  }

  def train(trainFile : String, testFile : String) : Unit = {
    val trainExamples = loadExamples(trainFile)
    val testExamples = loadExamples(testFile)
    train(trainExamples, testExamples)
 }
}
