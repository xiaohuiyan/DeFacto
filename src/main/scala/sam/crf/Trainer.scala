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
    model.train(args(2))
    model.saveWeights(args(3))
  }
}