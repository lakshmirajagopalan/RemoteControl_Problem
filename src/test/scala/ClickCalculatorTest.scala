import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{ShouldMatchers, FlatSpec}


class ClickCalculatorTest extends FlatSpec with ShouldMatchers {

  "Calculator" should "return the number of down arrow clicks to go from one Channel to next" in {
    val calculator = new ClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcDownClickCount(21, 10) should be(9)
    calculator.calcDownClickCount(10, 14) should be(7)
  }

  "Calculator" should "return the number of up arrow clicks to go from one Channel to next" in {
    val calculator = new ClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcUpClickCount(10, 21) should be(9)
    calculator.calcUpClickCount(21, 14) should be(4)
  }

  "Calculator" should "return the number of up or down arrow clicks to go from one Channel to next" in {
    val calculator = new ClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcUpDownClickCount(10, 21) should be(1)
    calculator.calcUpDownClickCount(21, 14) should be(4)
  }

//  "Calculator" should "return the number of clicks with the use of the back button to go from one Channel to next" in {
//    val calculator = new ClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
//    calculator.calcMinClicksWithBack(10, 21) should be(9)
//    calculator.calcMinClicksWithBack(21, 14) should be(4)
//  }

  "Calculator" should "return the number of clicks with the use of minChannel to go from one Channel to next" in {
    val calculator = new ClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcMinClicksWithMinChannel(12, 21) should be(3)
    calculator.calcMinClicksWithMinChannel(14, 19) should be(4)
  }

  it should "return the minimum number of clicks in moving from one Channel to another" in {
    val calculator = new ClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcMinClicksBetween(10,12) should be (2)
    calculator.calcMinClicksBetween(12, 15) should be (2)
    calculator.calcMinClicksBetween(12, 21) should be (2)
  }

  it should "return the minimum number of clicks in moving through the entire sequence of channels" in {
    val calculator = new ClickCalculator("10 21", "2 13 20", "5 10 12 15 12 21")
    calculator.calcMinClicks should be (9)
  }
}



