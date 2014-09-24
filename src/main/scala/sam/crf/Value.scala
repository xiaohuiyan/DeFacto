package sam.crf

/**
 * Created by samanzaroot on 9/23/14.
 */
/**
 * This is a variable value associated with a domain. Factors are on (and between) values.
 * @param domain
 * @tparam L
 */
abstract class Value[L <: Domain](val domain : Domain) {
  var value : domain.inner
}

abstract class DiscreteValue[L <: DiscreteDomain](dom : Domain) extends Value[L](dom)

class IntValue[IntDomain](val domain : IntDomain)
class RealValue[RealDomain](val domain : RealDomain)

class LabelValue[L <: LabelDomain](domain : L) extends DiscreteValue[L](domain) {
  var value = new OneHotVector(domain.length)
  var targetValue = new OneHotVector(domain.length)
  def category = domain.labels(value.index)
  val targetCategory = domain.labels(targetValue.index)
}

class FeatureValue[F <: FeaturesDomain](domain : F) extends DiscreteValue[F](domain) {
  val value = new SparseBinaryVector1(domain.length)
  def featureValues = value.activeElements.map(e => domain(e._1))
}

