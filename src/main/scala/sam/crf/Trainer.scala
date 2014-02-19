package sam.crf

object Trainer {

  def extractFeatures(line : String) : Array[Int] = line.split(",").drop(2).map(_.toInt)

  def main(args: Array[String]) {
    if(args.size < 4) {
      println("Usage: Trainer [FeatureDomain] [LabelDomain] [trainDirectory] [saveWeightDir]")
      sys.exit(0)
    }
    val model = new ChainModel(args(0), args(1).toInt, extractFeatures)
    //model.weights.index(4,0,10)
    //model.weights.index(0,3,0)
    val trainData = model.loadChains(args(2), {a : String => a.contains("train")}).take(60)
    val testData = model.loadChains(args(2), {a : String => a.contains("test")}).take(60)
    model.train(trainData, testData)
    model.saveWeights(args(3))
  }
}