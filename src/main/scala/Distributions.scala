import org.apache.commons.math3.distribution.{NormalDistribution, TDistribution}

object Distributions {

  def invNormal(area: Double): Double =
    new NormalDistribution().inverseCumulativeProbability(area)

  def invT(area: Double, df: Double): Double =
    new TDistribution(df).inverseCumulativeProbability(area)

}
