package sam.crf

import edu.umass.nlp.optimize.{IDifferentiableFn, CachingDifferentiableFn, LBFGSMinimizer}
import edu.umass.nlp.utils.{BasicPair, IPair}

/**
 * Created by samanzaroot on 9/19/14.
 */
abstract class ClassLBFGSTrainer(val model : Classifier, val trainData : Array[Chain], val testData : Array[Chain]) {
  val empirical = new Array[Double](model.weights.obsWeightsSize)
  var calculatedEmpirical = false

  def optimize() {
    val optimizerOpts = new LBFGSMinimizer.Opts()
    val fn = new CachingDifferentiableFn(new ObjFn(trainData))
    val optimizer = new LBFGSMinimizer()
    println(s"Running on ${trainData.size} amount of data.")
    println(s"Dimension is= ${model.weights.obsWeightsSize}")
    optimizer.minimize(fn, model.weights.getClassWeights, optimizerOpts)
    println("Restart minimization")
    val optRes = optimizer.minimize(fn, model.weights.getClassWeights, optimizerOpts)
    model.weights.setClassWeights(optRes.minArg)
  }

  def regularizationLikelihood(weights : Weights) : Double

  def regularizationGradient(weights : Weights) : Array[Double]

  class ObjFn(val data : Array[Chain]) extends IDifferentiableFn {

    def calcEmpirical(datums : Iterable[Chain]) {
      for (chain <- datums) {
        // Empirical
          chain.singleClique()
          val observationFactor = chain.head.observationFactors.head
          for(f <- observationFactor.observation.features.zipWithIndex) {
            val featureIndex = f._2
            val klass = observationFactor.label.targetValue
            val valueIndex = f._1-model.featuresDomain.features(f._2)(0)
            empirical(model.weights.index(featureIndex,klass,valueIndex)) += 1
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
        chain.singleClique()
        val infer = new Inference(chain).go()
        //val sumProduct = new LogSumProduct(chain).inferUpDown()
        logLike += chain.head.observationFactors.head.trueLog() // Sum the log potential value given the true labels and add to logLike
        logLike -= infer.logZ // Subtract the log of Z from the log likelihood
        //for (clique <- chain.iterator) {
          //val index = clique.index+1
          val s = infer.logTable
          for (i <- model.labelDomain.labels) {
            val observationFactor = chain.head.observationFactors.head
            // Node
            //for(observationFactor <- observationFactors) {
              //obsCount += 1
              var count = 0
              for (f <- observationFactor.observation.features) {
                val index = i
                val featureIndex = count
                val valueIndex = model.featuresDomain.featureIndex(count,f)
                grad(model.weights.index(featureIndex,index,valueIndex)) -= s(index)
                count += 1
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
      model.weights.getClassWeights.size
    }
  }
}
