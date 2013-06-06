/*import org.apache.commons.math.FunctionEvaluationException
import org.apache.commons.math.analysis.DifferentiableMultivariateRealFunction
import org.apache.commons.math.analysis.MultivariateRealFunction
import org.apache.commons.math.analysis.MultivariateVectorialFunction
import org.apache.commons.math.optimization.GoalType
import org.apache.commons.math.optimization.general.ConjugateGradientFormula
import org.apache.commons.math.optimization.general.NonLinearConjugateGradientOptimizer
import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import iitb.ugm.LogMath._

object MLR {
	var featuresDomain : FeaturesDomain = null
	var labelDomain : LabelDomain = null
	var testDir : String = null
	var count = 0
	
	def main(args: Array[String]) {
		featuresDomain = new FeaturesDomain(args(2))
		labelDomain = new LabelDomain(args(3).toInt)
		testDir = args(4)
		val d : Dataset = new Dataset(args(1), featuresDomain, labelDomain);
		val opt : NonLinearConjugateGradientOptimizer = new NonLinearConjugateGradientOptimizer(ConjugateGradientFormula.POLAK_RIBIERE);
		val weights : Array[Double] = opt.optimize(new LogLikelihood(d), GoalType.MAXIMIZE, new Array[Double](4070)).getPoint();
		/*for (w <- weights)
			println(w);*/
	}
	
	def unroll(weights : Array[Double]) : Weights = new Weights(featuresDomain,labelDomain).unroll(weights)
	
	def test(w : Array[Double]) : Double = {
		
		val weights = new Weights(featuresDomain,labelDomain).unroll(w)
		
		println(testDir)
		var testFiles = recursiveListFiles(new File(testDir)).filter(_.getAbsolutePath.contains("test"))
		
		val data = ArrayBuffer[ArrayBuffer[List[Int]]]()
		val ys = ArrayBuffer[ArrayBuffer[Int]]()
		var N : Int = 0    // total counts

		for(file <- testFiles) {
			var spY = ArrayBuffer[Int]()
			var sentence = ArrayBuffer[List[Int]]()
			for(line <- Source.fromFile(file.getAbsolutePath()).getLines()) {
				var split = line.split(",")
				var y1 = split(1)
				spY.append(y1.toInt)
				sentence.append( List(split(2).toInt, split(3).toInt, split(4).toInt, split(5).toInt, split(6).toInt) )
			}
			data.append(sentence)
			ys.append(spY)
			N += 1
		}
		
		var good = 0.0
		var T = 0.0
		for(s <- 0 until data.size) {
			for(t <- 0 until data(s).size) {
				val marginal = new Array[Double](labelDomain.until)
				for(l <- 1 to labelDomain.until) {
					marginal(l-1) = 0.0
					for(f <- 0 until featuresDomain.features.size) {
						marginal(l-1) += weights(l, f, data(s)(t)(f))
					}
					marginal(l-1) = exp(marginal(l-1))
				}
				if((marginal.indexOf(marginal.max)+1) == ys(s)(t)) good += 1
				T += 1
			}
		}
		good/T
	}
	
	def recursiveListFiles(f: File): Array[File] = {
	  val these = f.listFiles
	  these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
	}
}

class Dataset(dir : String, val featuresDomain : FeaturesDomain, val labelDomain : LabelDomain) {
	
	def recursiveListFiles(f: File): Array[File] = {
	  val these = f.listFiles
	  these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
	}
	
	val files = recursiveListFiles( new File(dir) ).filter(_.getAbsolutePath().contains("train")).take(30)
	
	val data = ArrayBuffer[ArrayBuffer[List[Int]]]()
	val ys = ArrayBuffer[ArrayBuffer[Int]]()
	var N : Int = 0    // total counts
	
	for(file <- files) {
		var spY = ArrayBuffer[Int]()
		var sentence = ArrayBuffer[List[Int]]()
		for(line <- Source.fromFile(file.getAbsolutePath()).getLines()) {
			var split = line.split(",")
			var y1 = split(1)
			spY.append(y1.toInt)
			sentence.append( List(split(2).toInt, split(3).toInt, split(4).toInt, split(5).toInt, split(6).toInt) )
		}
		data.append(sentence)
		ys.append(spY)
		N += 1
	}

}

class LogLikelihood(d : Dataset) extends DifferentiableMultivariateRealFunction {

	var data : Dataset = d
	
	
	// Calculates the log-likelihood
	override def value(point : Array[Double]) : Double = {
		println("LOG")
		assert(point.length == 4070)
		println("Accuracy: " + MLR.test(point) )
		val weights = new Weights(d.featuresDomain, d.labelDomain).unroll(point)
		var sum : Double = 0.0
		for(s <- 0 until data.data.size) {
			for(t <- 0 until data.data(s).size) {
				var plus = 0.0
				for(f <- 0 until data.featuresDomain.features.size) {
					plus += weights(data.ys(s)(t), f, data.data(s)(t)(f))
				}
				var minus = 0.0
				for(c <- 1 to data.labelDomain.until) {
					var minusOne = 0.0
					for(f <- 0 until data.featuresDomain.features.size) {
						minusOne += weights(c, f, data.data(s)(t)(f))
					}
					minus += exp(minusOne)
				}
				sum += (plus - log(minus.toFloat))
			}
		}
		(sum/data.N)
	}

	// Calculates partial derivative
	
	override def partialDerivative(k : Int) : MultivariateRealFunction = {
		return new PartialDer(data, k);
	}

	// Calculates gradient
	override def gradient() :MultivariateVectorialFunction = {
		return new Gradient(data);
	}

}

class PartialDer(d : Dataset, val cls : Int) extends MultivariateRealFunction {

	val data : Dataset = d
	var c : Int = cls // class number for which to calculate partial derivative
	
	override def value(point : Array[Double]) : Double = {
		assert(point.length == 4070);
		val weights = new Weights(data.featuresDomain,data.labelDomain).unroll(point)
		var sum = 0.0
		val idx = weights.decode(cls)
		//println(idx)
		var klass = idx._1
		var fe = idx._2
		var fv = idx._3
		var count = 0
		for(s <- 0 until data.data.size) {
			for(t <- 0 until data.data(s).size) {
				if(data.ys(s)(t) == klass && data.data(s)(t)(fe) == fv) count += 1
			}
		}
		var second = 0.0
		for(s <- 0 until data.data.size) {
			for(t <- 0 until data.data(s).size) {
				var minus = 0.0
				if(data.ys(s)(t) == klass && data.data(s)(t)(fe) == fv) {
					second = weights( klass, fe, data.data(s)(t)(fe) )
					for(c <- 1 to data.labelDomain.until) {
						minus = logSumExp( minus, weights( c, fe, data.data(s)(t)(fe) ) )
					}
				}
				second = second - minus
			}
		}
		//sprintln( count/data.N - (second) )
	    count/data.N - (second)
	}

}

class Gradient(d : Dataset) extends MultivariateVectorialFunction {

	MLR.count += 1
	val data : Dataset = d

	override def value(point : Array[Double]) : Array[Double] = {
		assert(point.length == 4070);
		println("GRAD: " + MLR.count)
		return (0 until 4070).map(new PartialDer(data,_).value(point)).toArray
	}

}
*/