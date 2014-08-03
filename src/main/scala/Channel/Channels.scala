

package Channel {


class Channels {
  private val BlockedChannels: Array[Int] = Array(0)
  private val MinChannel: Int = 0
  private val MaxChannel: Int = 0

}

object Channels {
  val InputChannelSequence: Array[Int] = Array(0)

  val channels = new Channels

  def getMinChannel(): Int = channels.MinChannel

  def getMaxChannel(): Int = channels.MaxChannel

  def parseInput(channelLimits: String, channelsBlocked: String, channelsToView: String) {
    val ChannelLimitsArray: Array[String] = channelLimits.split(" ")
    val ChannelsBlockedArray: Array[String] = channelsBlocked.split(" ")
    val ChannelsToViewArray: Array[String] = channelsToView.split(" ")

    val MinChannel: Int = ChannelLimitsArray(0).toInt
    val MaxChannel: Int = ChannelLimitsArray(1).toInt
    val totalChannelsBlocked: Int = ChannelsBlockedArray(0).toInt

    var i = 1
    while (i < ChannelsBlockedArray.length) {
      if (isWithinChannelLimits(ChannelsBlockedArray(i).toInt))
        channels.BlockedChannels(i - 1) = ChannelsBlockedArray(i).toInt;
      // else
      //throw("Channel outside Limits");
      i += 1
    }
    if (i != totalChannelsBlocked) {
      throw ("Total Channel Count Doesnt match the count in sequence");
    }
    channels.BlockedChannels(i) = 0;

    sort(channels.BlockedChannels);
    //Optimization can be added every time we have to check for Num of Blocked Elements between two channels.

    val totalChannelsToView = ChannelsToViewArray(0).toInt
    i = 1
    while (i < ChannelsToViewArray.length) {
      if (isWithinChannelLimits(ChannelsToViewArray(i).toInt))

        if (!isBlockedChannel(ChannelsToViewArray(i).toInt))
          channels.InputChannelSequence(i - 1) = ChannelsToViewArray(i).toInt;
      //          else
      //            throw("Blocked Channel");
      //        else
      //          throw("Channel outside Limits");
      i += 1
    }
    if (i != totalChannelsToView) {
      throw ("Total Channel Count Doesnt match the count in sequence");
    }
    channels.InputChannelSequence(i) = 0
  }

  def isWithinChannelLimits(ch: Int): Boolean = {
    if ((ch >= channels.MinChannel) && (ch <= channels.MaxChannel)) true
    else false
  }

  def isBlockedChannel(ch: Int): Boolean = {
    for (i <- 0 to channels.BlockedChannels.length - 2)
      if (channels.BlockedChannels(i) == ch)
        true
    false
  }

  //Returns the number of Blocked Channel.Channels between min and max inclusive of both.
  def getBlockedChannelCount(rangeMin: Int, rangeMax: Int): Int = {
    var n: Int = 0;
    if (rangeMin > rangeMax) //If direction is not from min to max, swap min & max and proceed. We need only the blocked channels between these two.
      (rangeMin, rangeMax) = swap(rangeMin, rangeMax)

    for (i <- 0 to channels.BlockedChannels.length - 2)
      for (j <- rangeMin to rangeMax)
        if (channels.BlockedChannels(i) == j)
          n += 1
    n
  }

  //Calculates clicks to reach from a to b by down arrow keys
  def calcDownClickCount(a: Int, b: Int): Int = {
    var aWrappedAround = a
    if (a <= b)
      aWrappedAround += (channels.MaxChannel - channels.MinChannel + 1)
    //For wraparound and the equal to is to simulate trying to get to the same channel via the down buttons
    val steps: Int = ((aWrappedAround - b) - getBlockedChannelCount(aWrappedAround, b))
    steps
  }


  //Calculates clicks to reach from a to b by up arrow keys
  def calcUpClickCount(a: Int, b: Int): Int = {
    var bWrappedAround = b
    if (a >= b) //For wraparound and the equal to is to simulate trying to get to the same channel via the up buttons.
      bWrappedAround += (channels.MaxChannel - channels.MinChannel + 1)
    val steps: Int = ((bWrappedAround - a) - getBlockedChannelCount(a, bWrappedAround))
    steps
  }

}

}


