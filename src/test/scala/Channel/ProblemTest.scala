package Channel

import org.scalatest.{ShouldMatchers, FlatSpec}

/**
 * Created by lakshmi on 4/8/14.
 */
class ProblemTest extends FlatSpec with ShouldMatchers {
  "Problem" should "return true if input channel is within limits" in {
    val problem = new Problem(Array(10, 13, 13, 14, 21), 10, 21, Array(13, 20))
    problem.isWithinChannelLimits(15) should be(true)
    problem.isWithinChannelLimits(22) should be(false)
  }

  it should "return true if input channel is Blocked" in {
    val problem = new Problem(Array(10, 13, 14, 21), 10, 21, Array(13, 20))
    problem.isBlocked(15) should be(false)
    problem.isBlocked(13) should be(true)
  }

  "Problem" should "return the number of blocked Channels between a range" in {
    val problem = new Problem(Array(10, 13, 14, 21), 10, 21, Array(13, 20))
    problem.getBlockedChannelCount(21, 21) should be(0)
    problem.getBlockedChannelCount(14, 20) should be(1)
  }

  //
  "Problem" should "return the min Channel" in {
    val problem = new Problem(Array(10, 13, 14, 21), 10, 21, Array(13, 20))
    problem.getMinChannel() should be(10)

  }
  //
  "Problem" should "return the max Channel" in {
    val problem = new Problem(Array(10, 13, 14, 21), 10, 21, Array(13, 20))
    problem.getMaxChannel() should be(21)
  }

  "Problem Factory" should "return a problem instance based on the input test case" in {
    val inputChannels = "4 10 13 14 21"
    val blockedChannels = "2 13 20"
    val channelLimits = "10 21"
    val problemInstance = Problem.getProblemInstance(channelLimits, blockedChannels, inputChannels)
    problemInstance.getMinChannel() should be(10)
    problemInstance.getMaxChannel() should be(21)
    problemInstance.getBlockedChannelSequence should equal(Array(13, 20))
    problemInstance.getViewingChannelSequence should equal(Array(10, 13, 14, 21))

  }
}

class ChannelClickCalculatorTest extends FlatSpec with ShouldMatchers {

  "Calculator" should "return the number of down arrow clicks to go from one Channel to next" in {
    val calculator = new ChannelClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcDownClickCount(21, 10) should be(9)
    calculator.calcDownClickCount(10, 14) should be(7)
  }

  "Calculator" should "return the number of up arrow clicks to go from one Channel to next" in {
    val calculator = new ChannelClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcUpClickCount(10, 21) should be(9)
    calculator.calcUpClickCount(21, 14) should be(4)
  }

  "Calculator" should "return the number of up or down arrow clicks to go from one Channel to next" in {
    val calculator = new ChannelClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcUpDownClickCount(10, 21) should be(9)
    calculator.calcUpDownClickCount(21, 14) should be(4)
  }

  "Calculator" should "return the number of clicks with the use of the back button to go from one Channel to next" in {
    val calculator = new ChannelClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcMinClicksWithBack(10, 21) should be(9)
    calculator.calcMinClicksWithBack(21, 14) should be(4)
  }

  "Calculator" should "return the number of clicks with the use of minChannel to go from one Channel to next" in {
    val calculator = new ChannelClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcMinClicksWithMinChannel(10, 21) should be(9)
    calculator.calcMinClicksWithMinChannel(21, 14) should be(4)
  }

  it should "return the minimum number of clicks in moving from one Channel to another" in {
    val calculator = new ChannelClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcMinClicksBetween(10,21) should be (1)
    calculator.calcMinClicksBetween(21,10) should be (1)
  }

  it should "return the minimum number of clicks in moving through the entire sequence of channels" in {
    val calculator = new ChannelClickCalculator("10 21", "2 13 20", "4 10 13 14 21")
    calculator.calcMinClicks should be (1)
  }
}



