package sam.crf

/**
 * Created by samanzaroot on 9/21/14.
 */
object TrainLogit {
  def extractFeatures(line : String) : Array[Int] = line.split(",").drop(2).map(_.toInt)

  def main(args: Array[String]) {
    if(args.size < 4) {
      println("Usage: Trainer [FeatureDomain] [LabelDomain] [trainDirectory] [saveWeightDir]")
      sys.exit(0)
    }
    val model = new Classifier(args(0), args(1).toInt, extractFeatures)
    //model.weights.index(4,0,10)
    //model.weights.index(0,3,0)
    val trainData = model.loadExamples(args(3))
    val testData = model.loadExamples(args(4))
    model.train(trainData, testData)
    //model.saveWeights(args(3))
  }
}
