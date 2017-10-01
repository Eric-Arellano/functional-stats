object SampleStats {

  def mean(seq: Seq[Double]): Option[Double] =
    if (seq.isEmpty) None else Some((seq.sum  * 1.0) / seq.length)

  def variance(seq: Seq[Double]): Option[Double] = {
    if (seq.isEmpty) None else {
      val xbar = mean(seq).get
      val squareSum = seq.foldLeft(0.0)((sum, xi) => sum + math.pow(xi - xbar, 2))
      Some(squareSum / (seq.length - 1.0))
    }
  }

  def stdDev(seq: Seq[Double]): Option[Double] =
    if (seq.isEmpty) None else Some(math.sqrt(variance(seq).get))

  def range(seq: Seq[Double]): Option[Double] =
    if (seq.isEmpty) None else Some(seq.max - seq.min)
}
