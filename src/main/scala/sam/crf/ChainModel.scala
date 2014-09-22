package sam.crf

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.File
import scala.collection.mutable

class ChainModel(domainFile : String, labelDomainSize : Int, ff : (String=>Array[Int])) {
  
  val featuresDomain = new FeaturesDomain(domainFile)
  val labelDomain = new LabelDomain(labelDomainSize)
  
  val weights = new Weights(featuresDomain, labelDomain)
  val chains = new ArrayBuffer[Chain]()
  var datasetSize = 0

	def recursiveListFiles(f: File): Array[File] = {
	  val these = f.listFiles
	  if(!these.filter(_.isDirectory).isEmpty)
	  	these.filter(_.isDirectory).flatMap(recursiveListFiles)
	  else these
	}
	
	def loadChains(dir : String, filter : String => Boolean = {a : String => true}) : Array[Chain] = {
    val arraybuffer = new ArrayBuffer[Chain]()
		val files = recursiveListFiles(new File(dir))
    var count = 0
		for(file <- files; if filter(file.getAbsolutePath)) {
      arraybuffer += new Chain(weights,ff).loadChain(file.getAbsolutePath())
      count += 1
    }
    arraybuffer.toArray
	}

  def loadChainsTrain(dir : String) {
    val files = recursiveListFiles(new File(dir))
    for(file <- files)
      chains += new Chain(weights,ff).loadChain(file.getAbsolutePath())
  }

  def setTransitionWeights(file : String) {
		var count = 0
    var countj = 0
		for(line <- Source.fromFile(file).getLines()) {
			weights.transWeights(count)(countj) = line.toDouble
			count += 1
      if(count % labelDomainSize == 0) { countj += 1; count = 0 }
		}
	}
	
	def setObservationWeights(file : String) {
		val lines = Source.fromFile(file).getLines().toArray
		var count = 0
		for(f <-  0 until featuresDomain.features.size) {
			for(v <- 0 until featuresDomain.features(f).size ) {
        for(k <- 0 until labelDomain.until) {
				  weights.obsWeights(f)(v)(k) = lines(count).toDouble
				  count += 1
        }
			}
		}
	}
	
	def test(testChains : Array[Chain]) {
    for(chain <- testChains) {
			val sp = new LogSumProduct(chain)
			sp.inferUpDown()
			sp.setToMaxMarginal()
			sp.printMarginals(1)
      sp.printMarginals(2)
      println("")
		}
	}

  def evaluate(testChains : Array[Chain]) : Double = {
    var correct = 0
    var total = 0
    for(chain <- testChains) {
      val sp = new LogSumProduct(chain)
      sp.inferUpDown()
      sp.setToMaxMarginal()
      for(label <- chain.labelIterator) {
        total += 1
        if(label.value == label.targetValue) correct += 1
      }
    }
    correct.toDouble / total
  }


  def evaluate : Double = {
    var correct = 0
    var total = 0
    for(chain <- chains) {
      val sp = new LogSumProduct(chain)
      sp.inferUpDown()
      sp.setToMaxMarginal()
      for(label <- chain.labelIterator) {
        total += 1
        if(label.value == label.targetValue) correct += 1
      }
    }
    correct.toDouble / total
  }


  def train(train : Array[Chain], test : Array[Chain]) {
    (new LBFGSTrainer(this, train, test) with L2Regularization).optimize()
  }

  def saveWeights(dir : String) {
    // Save the weights to disk
  }

}
