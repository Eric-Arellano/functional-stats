import org.scalatest._

class ConfidenceIntervalsTest extends FlatSpec with Matchers {

  "A two-sided CI" should "use the normal distribution when large enough" in {
//    val seq = Seq.fill(10)(3.4, 1.3, 4.1, -12.42).flatten
//    val mean = SampleStats.mean(seq).get
//    ConfidenceIntervals.twoSidedCI(seq, 0.1).get should equal (mean - 1.1206 +- 0.0001, mean + 1.1206 +- 0.0001)
  }

  "it" should "use the T distribution when smaller than 30 elements" in {
        val seq = Seq(3.4, 1.3, 4.1, -12.42)
        val mean = SampleStats.mean(seq).get
        ConfidenceIntervals.twoSidedCI(seq, 0.1).get should equal (mean - 9.14083793 +- 0.1, mean + 9.14083793 +- 0.1)
  }

  "it" should "not produce a two-sided CI for empty sequences" in {
    val seq = Seq.empty[Double]
    ConfidenceIntervals.twoSidedCI(seq, 0.1).isEmpty should be (true)
  }

}
