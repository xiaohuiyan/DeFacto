package sam.crf

trait Regularization {
  def regularizationLikelihood(weights : Weights) : Double

  def regularizationGradient(weights : Weights) : Array[Double]
}

trait NoRegularization extends Regularization {
  def regularizationLikelihood(weights : Weights) : Double = 0.0

  def regularizationGradient(weights : Weights) : Array[Double] = new Array[Double](weights.dimension)
}

trait L2Regularization extends Regularization {
  var sigma = 10.0
  lazy val sigmaSquared = math.pow(sigma, 2)
  def regularizationLikelihood(weights : Weights) : Double = {
    var sum = 0.0
    for(weight <- weights.getWeights) {
      sum += math.pow(weight,2)/(2.0*sigmaSquared)
    }
    sum
  }

  def regularizationGradient(weights : Weights) : Array[Double] = {
    val returns = new Array[Double](weights.dimension)
    var count = 0
    for(weight <- weights.getWeights) {
      returns(count) = weight/sigmaSquared
      count += 1
    }
    returns
  }
}
