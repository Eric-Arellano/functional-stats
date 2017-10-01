import math._

object ConfidenceIntervals {

  def twoSidedCI(seq: Seq[Double], alpha: Double): Option[(Double, Double)] = {
    if (seq.isEmpty) None else {
      val n = seq.length
      val xbar = SampleStats.mean(seq).get
      val s = SampleStats.stdDev(seq).get
      if (n < 30) {
        Some((xbar - Distributions.invT(1 - alpha/2, n - 1) * s / sqrt(n),
          xbar + Distributions.invT(1 - alpha/2, n - 1) * s / sqrt(n)))
      } else {
        Some((xbar - Distributions.invNormal(1 - alpha/2) * s / sqrt(n),
          xbar + Distributions.invNormal(1 - alpha/2) * s / sqrt(n)))
      }
    }
  }

  def lowerCI(seq: Seq[Double], alpha: Double): Option[Double] = {
    if (seq.isEmpty) None else {
      val n = seq.length
      val xbar = SampleStats.mean(seq).get
      val s = SampleStats.stdDev(seq).get
      if (n < 30) {
        Some(xbar - Distributions.invT(1 - alpha, n - 1) * s / sqrt(n))
      } else {
        Some(xbar - Distributions.invNormal(1 - alpha) * s / sqrt(n))
      }
    }
  }

  def upperCI(seq: Seq[Double], alpha: Double): Option[Double] = {
    if (seq.isEmpty) None else {
      val n = seq.length
      val xbar = SampleStats.mean(seq).get
      val s = SampleStats.stdDev(seq).get
      if (n < 30) {
        Some(xbar + Distributions.invT(1 - alpha, n - 1) * s / sqrt(n))
      } else {
        Some(xbar + Distributions.invNormal(1 - alpha) * s / sqrt(n))
      }
    }
  }

}
