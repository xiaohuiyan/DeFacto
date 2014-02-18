package sam.crf

import edu.umass.nlp.optimize.{LBFGSMinimizer, CachingDifferentiableFn, IDifferentiableFn}
import edu.umass.nlp.ml.sequence.CRF.Opts
import scala.Double
import edu.umass.nlp.utils.{BasicPair, IPair}
import edu.umass.nlp.ml.sequence.{ForwardBackwards, Transition}
import scala.collection.mutable.ArrayBuffer


class LBFGSTrainer(val model : ChainModel, trainData : Array[Chain], testData : Array[Chain]) {

  val emperical = new Array[Double](model.weights.dimension)
  var calculatedEmperical = false

  def optimize() {
    val optimizerOpts = new LBFGSMinimizer.Opts()
    val fn = new CachingDifferentiableFn(new ObjFn(trainData))
    val optimizer = new LBFGSMinimizer()
    println(s"Running on ${trainData.size} amount of data.")
    println(s"Dimension is= ${model.weights.dimension}")
    println("One")
    val optRes = optimizer.minimize(fn, model.weights.getWeights, optimizerOpts)
    println("Two")
    val optRes2 = optimizer.minimize(fn, model.weights.getWeights, optimizerOpts)
    println("Three")
    val optRes3 = optimizer.minimize(fn, model.weights.getWeights, optimizerOpts)
    println("Four")
    val optRes4 = optimizer.minimize(fn, model.weights.getWeights, optimizerOpts)
    model.weights.setWeights(optRes4.minArg)
  }


  class ObjFn(val data : Array[Chain]) extends IDifferentiableFn {

    def calcEmperical(datums : Iterable[Chain]) {
      for (chain <- datums) {
        chain.makeCliqueTree()
        // Empirical
        for (clique <- chain.iterator) {
          val observationFactor = clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head
          for(f <- observationFactor.observation.features.zipWithIndex) {
            val featureIndex = f._2
            val klass = observationFactor.label.targetValue-1
            val valueIndex = f._1-model.featuresDomain.features(f._2)(0)
            emperical(model.weights.index(featureIndex,klass,valueIndex)) += 1
          }
          val i = observationFactor.label.targetValue
          val secondFactor = if(clique.next != null) clique.next.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head else clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).last
          val j = secondFactor.label.targetValue
          emperical(model.weights.transIndex(i-1,j-1)) += 1.0
        }
      }
      calculatedEmperical = true
    }

    def compute(datums : Iterable[Chain]) : IPair[java.lang.Double, Array[Double]] =  {
      var logLike = 0.0
      val grad = new Array[Double](model.weights.dimension)
      //  Obj Fn
      //  logLike += log P( correct-sequence | input)  =  sum( true-log-pots ) - logZ
      //  grad +=  (Empirical-Feat-Counts - Expected-Feat-Counts)
      if(!calculatedEmperical)
        calcEmperical(datums)

      for(i <- 0 until grad.length)
          grad(i) = emperical(i)
        // Expected
        for(chain <- datums) {
          val sp = new LogSumProduct(chain).inferUpDown()
          logLike += chain.iterator.map( _.trueLog() ).sum // Sum the log potential value given the true labels and add to logLike
          logLike -= sp.logZ // Subtract the log of Z from the log likelyhood
          for (clique <- chain.iterator) {
          val index = clique.index+1
          val s = sp(index)
          val trans = sp(clique.i, clique.i+1)
          for (i <- model.labelDomain.labels) {
            val observationFactor = clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head
            // Node
            var count = 0
            for (f <- observationFactor.observation.features) {
              val spIndex = i-1
              val featureIndex = count
              val klass = i-1
              val valueIndex = model.featuresDomain.features(count).indexOf(f)
              grad(model.weights.index(featureIndex,klass,valueIndex)) -=  s(spIndex)
              count += 1
            }
            // Transition
            for (j <- model.labelDomain.labels) {
              val iIndex = i-1
              val jIndex = j-1
              val index = iIndex*model.labelDomain.until+jIndex
              grad(model.weights.transIndex(i-1,j-1)) -= trans(index)
            }
          }
        }
      }
      BasicPair.make(logLike, grad);
    }

    def computeAt(x : Array[Double]) : IPair[java.lang.Double, Array[Double]] = {
      model.weights.setWeights(x)
      var logLike = 0.0
      var grad : Array[Double] = null

      val res = compute(data);
      logLike = res.getFirst();
      grad = res.getSecond();


      logLike *= -1.0;
      grad = grad.map( _ * -1.0)

            println("Train Accuracy:" + model.evaluate(trainData))
            println("Test Accuracy:" + model.evaluate(testData))

            println("Grad: " + (grad).mkString(", "))
            println("Weights: " + x.mkString(", "))
            println("Value: " + logLike)

      BasicPair.make(logLike, grad)
    }


    def getDimension() : Int = {
      model.weights.getWeights.size
    }
  }

}


