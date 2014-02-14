package sam.crf

import edu.umass.nlp.optimize.{LBFGSMinimizer, CachingDifferentiableFn, IDifferentiableFn}
import edu.umass.nlp.ml.sequence.CRF.Opts
import scala.Double
import edu.umass.nlp.utils.{BasicPair, IPair}
import edu.umass.nlp.ml.sequence.{ForwardBackwards, Transition}
import scala.collection.mutable.ArrayBuffer


class LBFGSTrainer(val model : ChainModel) {

  def optimize() {
    val optimizerOpts = new LBFGSMinimizer.Opts()
    val fn = new CachingDifferentiableFn(new ObjFn(model.chains))
    val optimizer = new LBFGSMinimizer()
    val optRes = optimizer.minimize(fn, model.weights.getWeights, optimizerOpts)
    model.weights.setWeights(optRes.minArg)
  }


  class ObjFn(val data : ArrayBuffer[Chain]) extends IDifferentiableFn {

    def compute(datums : Iterable[Chain]) : IPair[java.lang.Double, Array[Double]] =  {
      var logLike = 0.0
      val grad = new Array[Double](model.weights.dimension)
      var count = 0
      println(s"Running on ${datums.size} amount of data.")
      println(s"Dimention is= ${grad.size}")
      for (chain <- datums) {
        println("data item: " + count)
        count += 1
        val sp = new LogSumProduct(chain).inferUpDown()
        //  Obj Fn
        //  logLike += log P( correct-sequence | input)  =  sum( true-log-pots ) - logZ
        //  grad +=  (Empirical-Feat-Counts - Expected-Feat-Counts)

        // Objective Component
        logLike += chain.iterator.map( _.trueLog() ).sum // Sum the log potential value given the true labels and add to logLike
        logLike -= sp.logZ // Subtract the log of Z from the log likelyhood
        // Graident Component
        // Empirical
        for (clique <- chain.iterator) {
          val observationFactor = clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head
          for (i <- model.labelDomain.labels) {
            // Empirical State Feats
            for(f <- observationFactor.observation.features.zipWithIndex) {
              val value = if(i==observationFactor.label.targetValue) 1.0 else 0.0
              val featureIndex = f._2
              val klass = i-1
              val valueIndex = f._1-model.featuresDomain.features(f._2)(0)
              //println(s"Index: ${model.weights.index(featureIndex,klass,valueIndex)}")
              grad(model.weights.index(featureIndex,klass,valueIndex)) += value
            }
            // Empirical Trans Feat
            for(j <- model.labelDomain.labels) {
              val secondFactor = if(clique.next != null) clique.next.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head else clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).last
              val value = if(observationFactor.label.targetValue==i && secondFactor.label.targetValue==j) 1.0 else 0.0
              grad(model.labelDomain.until*(j-1)+(i-1) + model.weights.obsWeightsSize) += value
              //println("Index: " + (model.labelDomain.until*(i-1)+(j-1) + model.weights.obsWeightsSize))
            }
          }
        }
        // Expected
        for (clique <- chain.iterator) {
          for (i <- model.labelDomain.labels) {
            val observationFactor = clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head
            // Node
            for (f <- observationFactor.observation.features.zipWithIndex) {
              for(fv <- model.featuresDomain.features(f._2).zipWithIndex) {
                val index = clique.index+1
                val s = sp(index)
                val spIndex = i-1
                val gradI = f._2*(i-1)+fv._2
                val featureIndex = f._2
                val klass = i-1
                val valueIndex = fv._2
                if(f._1==fv._1)  grad(model.weights.index(featureIndex,klass,valueIndex)) -=  sp(index)(spIndex)
                if(sp(index)(spIndex).isNaN)
                  println("NaN")
              }
            }
            // Transition
            for (j <- model.labelDomain.labels) {
              grad(model.labelDomain.until*(j-1)+(i-1) + model.weights.obsWeightsSize) -= sp(clique.i, clique.i+1)(i+j)
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

            println("Num Feats: " + x.length);
            println("Grad: " + (grad).mkString(", "))
            println("Weights: " + x.mkString(", "))
            println("Value: " + logLike)
            println("Done with Computing Objective");

      BasicPair.make(logLike, grad)
    }


    def getDimension() : Int = {
      model.weights.getWeights.size
    }
  }

}


