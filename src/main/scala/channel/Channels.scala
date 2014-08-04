package channel


class Problem(channelsToView: Array[Int], minChannel: Int, maxChannel: Int, blockedChannels: Array[Int]) {

  def isWithinChannelLimits(ch: Int) = (ch >= minChannel) && (ch <= maxChannel)

  def isBlocked(ch: Int): Boolean = blockedChannels contains ch

  def getMinChannel(): Int = minChannel

  def getMaxChannel(): Int = maxChannel

  def getViewingChannelSequence: Array[Int] = channelsToView

  def getBlockedChannelSequence: Array[Int] = blockedChannels

  //Returns the number of Blocked Channel.Channels between min and max inclusive of both.
  def getBlockedChannelCount(rangeMin: Int, rangeMax: Int): Int = {
    blockedChannels.filter(b => b >= rangeMin && b <= rangeMax).size
  }


}
object Problem {

  def getProblemInstance(channelLimits: String, channelsBlocked: String, channelsToView: String):Problem = {
    val ChannelLimitsArray: Array[String] = channelLimits.split(" ")
    val ChannelsBlockedArray: Array[String] = channelsBlocked.split(" ")
    val ChannelsToViewArray: Array[String] = channelsToView.split(" ")

    val MinChannel: Int = ChannelLimitsArray(0).toInt
    val MaxChannel: Int = ChannelLimitsArray(1).toInt
    val totalChannelsBlocked: Int = ChannelsBlockedArray(0).toInt

    val blockedChannels: Array[Int] = ChannelsBlockedArray.tail.map(_.toInt)

    val totalChannelsToView = ChannelsToViewArray(0).toInt

    val inputSequenceToView: Array[Int] = ChannelsToViewArray.tail.map(_.toInt)

    new Problem(inputSequenceToView, MinChannel, MaxChannel, blockedChannels)
  }

}




//
//





