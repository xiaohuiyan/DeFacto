package sam.crf

import edu.umass.nlp.optimize.{LBFGSMinimizer, CachingDifferentiableFn, IDifferentiableFn}
import edu.umass.nlp.ml.sequence.CRF.Opts
import scala.Double
import edu.umass.nlp.utils.{BasicPair, IPair}
import edu.umass.nlp.ml.sequence.{ForwardBackwards, Transition}
import scala.collection.mutable.ArrayBuffer


class LBFGSTrainer(val model : Model) {

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
      for (chain <- datums) {
        val sp = new SumProduct(chain).inferUpDown()
        //  Obj Fn
        //  logLike += log P( correct-sequence | input)  =  sum( true-log-pots ) - logZ
        //  grad +=  (Empirical-Feat-Counts - Expected-Feat-Counts)

        // Objective Component
        logLike += chain.map( _.trueLog() ).sum // Sum the log potential value given the true labels and add to logLike
        logLike -= math.log(sp.Z) // Subtract the log of Z from the loglikeliehood

        // Graident Component
        // Empirical
        for (clique <- chain) {
          val observationFactor = clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head
          for (i <- model.labelDomain.labels) {
            // Empirical State Feats
            for(f <- observationFactor.observation.features.zipWithIndex) {
              val value = if(i==observationFactor.label.targetValue) f._1.toDouble else 0.0
              grad(f._2*(i-1)/*TODO: Set to label index. Include label as factor in index.*/) += value
            }
            // Empirical Trans Feat
            for(j <- model.labelDomain.labels) {
              val secondFactor = if(clique.next != null) clique.next.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head else clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).last
              val value = if(observationFactor.label.targetValue==i && secondFactor.label.targetValue==j) 1.0 else 0.0
              grad((i-1)*(j-1) + model.featuresDomain.size*model.labelDomain.labels.size) += value
            }
          }
        }
        // Expected
        for (clique <- chain) {
          for (i <- model.labelDomain.labels) {
            val observationFactor = clique.factors.filter(_.isInstanceOf[ObservationFactor]).map(_.asInstanceOf[ObservationFactor]).head
            // Node
            for (f <- observationFactor.observation.features.zipWithIndex) {
              grad(f._2*(i-1)/*TODO: Set to label index*/) -=  f._1*sp(clique.index)(i)
            }
            // Transition
            for (j <- model.labelDomain.labels) {
              grad((i-1)*(j-1) + model.featuresDomain.size/*TODO: Set to correct weight index*/) -= sp(clique.i, clique.i+1)(i+j)
            }
          }
        }
      }
      BasicPair.make(logLike, grad);
    }

    def computeAt(x : Array[Double]) : IPair[java.lang.Double, Array[Double]] = {
      model.weights.setWeights(x);
      var logLike = 0.0
      var grad : Array[Double] = null

      val res = compute(data);
      logLike = res.getFirst();
      grad = res.getSecond();


      logLike *= -1.0;
      grad = grad.map( _ * -1.0)

      /*if (opts.regularizer != null) {
        IPair<Double, double[]> res = opts.regularizer.apply(x);
        logLike += res.getFirst();
        DoubleArrays.addInPlace(grad, res.getSecond());
      }*/
      //      for (int i = 0; i < x.length; i++) {
      //        double w = x[i];
      //        logLike += (0.5 * w * w) / sigmaSquared;
      //        grad[i] += w / sigmaSquared;
      //      }
      //      logger.info("Num Feats: " + x.length);
      //      logger.info("Grad: " + DoubleArrays.toString(grad,10));
      //      logger.info("Weights: " + DoubleArrays.toString(x,10));
      //      logger.info("Value: " + logLike);
      //if (true) System.exit(0);
      println("Done with Computing Objective");

      BasicPair.make(logLike, grad)
    }


    def getDimension() : Int = {
      model.weights.getWeights.size
    }
  }

}


