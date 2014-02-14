package sam.crf

object Part1 {
	
	def extractFeatures(line : String) : Array[Int] = {
		val split = line.split(",")
		Array[Int](split(0).toInt)
	}
	
	def main(args: Array[String]) {
		if(args.size < 6) {
			println("Usage: Part1 [featuresDomain] [sizeOfLabel] [testDirectory] [transitionWeights] [observationWeights]")
			sys.exit(0)
		}
    val model = new ChainModel(args(1), args(2).toInt, extractFeatures)
		model.setTransitionWeights(args(4))
		model.setObservationWeights(args(5))
    //model.train(new SampleRankTrainer)
		model.test(args(3))   		
	}	
}
