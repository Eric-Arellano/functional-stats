import org.scalatest._

class SampleStatsTest extends FlatSpec with Matchers {

  "Sample mean" should "be the sum of its x elements divided by the number of elements" in {
    val seq = Seq(3.0, 1.0, 4.0, 2.0)
    SampleStats.mean(seq).get should equal (2.5)
  }

  "it" should "not produce a sample mean for empty sequences" in {
    val seq = Seq.empty[Double]
    SampleStats.mean(seq).isEmpty should be (true)
  }

  "Sample variance" should "be the sum of (x_1 - x_bar)^2 divided by the number of elements - 1" in {
    val seq = Seq(3.0, 1.0, 4.0)
    SampleStats.variance(seq).get should equal (2.33333 +- 0.005)
  }

  "it" should "not produce a sample variance for empty sequences" in {
    val seq = Seq.empty[Double]
    SampleStats.variance(seq).isEmpty should be (true)
  }

  "Sample standard deviation" should "be the square root of the variance" in {
    val seq = Seq(3.0, 1.0, 4.0)
    SampleStats.stdDev(seq).get should equal (1.5275 +- 0.005)
  }

  "it" should "not produce a sample standard deviation for empty sequences" in {
    val seq = Seq.empty[Double]
    SampleStats.stdDev(seq).isEmpty should be (true)
  }

  "Sample range" should "be the max value minus the minimum value" in {
    val seq = Seq(3.0, 1.0, 4.0)
    SampleStats.range(seq).get should equal (3.0)
  }

  "it" should "not produce a sample range for empty sequences" in {
    val seq = Seq.empty[Double]
    SampleStats.range(seq).isEmpty should be (true)
  }

}
