package sam.crf

import edu.umass.nlp.optimize.{LBFGSMinimizer, CachingDifferentiableFn, IDifferentiableFn}
import edu.umass.nlp.ml.sequence.CRF.Opts
import scala.{Array, Double}
import edu.umass.nlp.utils.{BasicPair, IPair}
import edu.umass.nlp.ml.sequence.{ForwardBackwards, Transition}
import scala.collection.mutable.ArrayBuffer


abstract class LBFGSTrainer(val model : ChainModel, trainData : Array[Chain], testData : Array[Chain]) {

  val empirical = new Array[Double](model.weights.dimension)
  var calculatedEmpirical = false

  def optimize() {
    val optimizerOpts = new LBFGSMinimizer.Opts()
    val fn = new CachingDifferentiableFn(new ObjFn(trainData))
    val optimizer = new LBFGSMinimizer()
    println(s"Running on ${trainData.size} amount of data.")
    println(s"Dimension is= ${model.weights.dimension}")
    optimizer.minimize(fn, model.weights.getWeights, optimizerOpts)
    println("Restart minimization")
    val optRes = optimizer.minimize(fn, model.weights.getWeights, optimizerOpts)
    model.weights.setWeights(optRes.minArg)
  }

  def regularizationLikelihood(weights : Weights) : Double

  def regularizationGradient(weights : Weights) : Array[Double]

  class ObjFn(val data : Array[Chain]) extends IDifferentiableFn {

    def calcEmpirical(datums : Iterable[Chain]) {
      for (chain <- datums) {
        chain.makeCliqueTree()
        chain.logComputeCliques()
        // Empirical
        for (clique <- chain.iterator) {
          for(observationFactor <- clique.observationFactors; f <- observationFactor.observation.features.zipWithIndex) {
            val featureIndex = f._2
            val klass = observationFactor.label.targetValue-1
            val valueIndex = f._1-model.featuresDomain.features(f._2)(0)
            empirical(model.weights.index(featureIndex,klass,valueIndex)) += 1
          }
          empirical(model.weights.transIndex(clique.transFactor.left.targetValue-1,clique.transFactor.right.targetValue-1)) += 1.0
        }
      }
      calculatedEmpirical = true
    }

    def compute(datums : Iterable[Chain]) : IPair[java.lang.Double, Array[Double]] =  {
      var logLike = 0.0
      //  Obj Fn
      //  logLike += log P( correct-sequence | input)  =  sum( true-log-pots ) - logZ
      //  grad +=  (Empirical-Feat-Counts - Expected-Feat-Counts)
      if(!calculatedEmpirical)
        calcEmpirical(datums)

      val grad = empirical.clone()
        // Expected
        for(chain <- datums) {
          val sumProduct = new LogSumProduct(chain).inferUpDown()
          logLike += chain.iterator.map( _.trueLog() ).sum // Sum the log potential value given the true labels and add to logLike
          logLike -= sumProduct.logZ // Subtract the log of Z from the log likelihood
          for (clique <- chain.iterator) {
          val index = clique.index+1
          val s = sumProduct(index)
          val s2 = if(clique.observationFactors.length == 2) sumProduct(index+1) else Array[Double]()
          val trans = sumProduct(clique.i, clique.i+1)
          for (i <- model.labelDomain.labels) {
            val observationFactors = clique.observationFactors
            // Node
            var obsCount = 0
            for(observationFactor <- observationFactors) {
              obsCount += 1
              var count = 0
              for (f <- observationFactor.observation.features) {
                val spIndex = i-1
                val featureIndex = count
                val klass = i-1
                val valueIndex = model.featuresDomain.featureIndex(count,f)
                grad(model.weights.index(featureIndex,klass,valueIndex)) -=  (if(obsCount == 2) s2(spIndex) else s(spIndex))
                count += 1
              }
            }
            // Transition
            for (j <- model.labelDomain.labels)
              grad(model.weights.transIndex(i-1,j-1)) -= trans.head(i-1)(j-1)
          }
        }
      }
      logLike -= regularizationLikelihood(model.weights)
      val rg = regularizationGradient(model.weights)
      var c = 0
      while(c < grad.length) {
        grad(c) -= rg(c)
        c += 1
      }
      BasicPair.make(logLike, grad)
    }

    def computeAt(x : Array[Double]) : IPair[java.lang.Double, Array[Double]] = {
      model.weights.setWeights(x)
      var logLike = 0.0
      var grad : Array[Double] = null
      val start = System.currentTimeMillis()
      val res = compute(data);
      val takes = (System.currentTimeMillis() - start).toDouble / 1000

      println("Compute: " + data.size + " in " + takes + " sec")
      println("Is: " + data.size/takes + " examples/sec")
      println("Is: " + takes/data.size + " sec/example")

      logLike = res.getFirst
      grad = res.getSecond


      logLike *= -1.0;
      grad = grad.map( _ * -1.0)

      println("Train Accuracy:" + model.evaluate(trainData))
      println("Test Accuracy:" + model.evaluate(testData))

      println("Grad: " + grad.mkString(", "))
      println("Weights: " + x.mkString(", "))
      println("Value: " + logLike)

      BasicPair.make(logLike, grad)
    }


    def getDimension() : Int = {
      model.weights.getWeights.size
    }
  }

}


