import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.File
class Model(domainFile : String, labelDomainSize : Int, ff : (String=>Array[Int])) {
	val featuresDomain = new FeaturesDomain(domainFile)
	val labelDomain = new LabelDomain(labelDomainSize)
	
	val weights = new Weights(featuresDomain, labelDomain)
	val chains = new ArrayBuffer[Chain]()
	
	def recursiveListFiles(f: File): Array[File] = {
	  val these = f.listFiles
	  if(!these.filter(_.isDirectory).isEmpty)
	  	these.filter(_.isDirectory).flatMap(recursiveListFiles)
	  else these
	}
	
	def loadChains(dir : String) {
		val files = recursiveListFiles(new File(dir))
		for(file <- files)
			chains += new Chain(weights,ff).loadChain(file.getAbsolutePath())
	}
	
	def setTransitionWeights(file : String) {
		var count = 0
		for(line <- Source.fromFile(file).getLines()) {
			weights.transWeights(count) = line.toDouble
			count += 1 
		}
	}
	
	def setObservationWeights(file : String) {
		val lines = Source.fromFile(file).getLines().toArray
		var count = 0
		for(f <-  0 until featuresDomain.features.size) {
			for(v <- 0 until (featuresDomain.features(f).size * labelDomain.until) ) {
				weights.obsWeights(f)(v) = lines(count).toDouble
				count += 1
			}
		}
	}
	
	def test(dir : String) {
		loadChains(dir)
		for(chain <- chains) {
			val sp = new SumProduct(chain)
			sp.inferUpDown()
			sp.setToMaxMarginal()
			sp.printMarginals(1)
			println("")
		}
	}
}

class FeaturesDomain(val file : String) {
	var features = new ArrayBuffer[Array[Int]]()
	for(line <- Source.fromFile(file).getLines()) {
		var split = line.split("->")
		features.append((split(0).toInt to split(1).toInt).toArray)
	}
	def size = features.size
}

class LabelDomain(val until : Int) {
	var labels = 1 to until
}

class Weights(val features : FeaturesDomain, val labels : LabelDomain) {
	val obsWeights = new Array[Array[Double]](features.size)
	var count = 0
	for(feature <- features.features) {
		obsWeights(count) = new Array[Double](feature.size*labels.until)
	}
	
	def unroll(weights : Array[Double]) : Weights= {
		var c = 0
		var f = 0
		for(i <- 0 until features.features.size) {
			var feature = ArrayBuffer[Double]()
			for(j <- 0 until features.features(i).size * labels.until) {
				feature.append(weights(c))
				c += 1
			}
			obsWeights(f) = feature.toArray
			f += 1
		}
		this
	}
	
	val transWeights = new Array[Double](labels.until*labels.until)
	
	def decode(i : Int) : (Int, Int, Int) = {
		var klass = 1
		var feat = 0
		var v = 0
		var count = 0
		var in = 0
		while(count < i) {
			if( in-1 == obsWeights(feat).size) {
				feat += 1
				in = 0
			} else {
				in+=1
			}
			count += 1
		}
		((in/features.features(feat).size)+1, feat, in%features.features(feat).size+features.features(feat)(0))
	}
	
	def apply(klass : Int, feature : Int, value : Int) : Double = {
		//println(obsWeights(feature).size)
		//println("class: " + klass)
		//println("feature: " + feature)
		//println("value: " + value)
		//println( ((klass-1)*labels.until) + (value-features.features(feature)(0)) )
		obsWeights(feature)( ((klass-1)*features.features(feature).size) + (value-features.features(feature)(0)) )
	}
	
	def apply(y1 : Int, yPlus : Int) : Double = {
		transWeights( ((yPlus-1)*labels.until)+(y1-1) )
	}
	
	def print() {
		for(f <- obsWeights) {
			for(i <- f) {
				println(i)
			}
			println("")
		}
		println("Transition:")
		for(t <- transWeights) {
			println(t)
		}
		println("")
	}
	
}