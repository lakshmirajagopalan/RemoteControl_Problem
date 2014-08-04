package channel

class ClickCalculator(channelLimits: String, channelsBlocked: String, channelsToView: String) {
  private var curIndex: Int = 0
  //this is cur position in the array sequence
  private var prevChannel: Int = -1 //The previous channel visited before this.
  //This may not be equal to what is given by InputSequence[curInd -1]  in the case of using up & dwon buttons.

  //3 variables to hold the prev channel for the upDown Arrow key press based on the combination.
  //After calculating the minimum clicks out of these, the prevChannel is updated
  private var prevForNextIterationByBackChannelUpDownClick: Int = -1
  private var prevForNextIterationByMinChannelDownClick: Int = -1
  private var prevForNextIterationByUpDownClick: Int = -1


  private val problemInstance:Problem = Problem.getProblemInstance(channelLimits, channelsBlocked, channelsToView)

  //Calculates clicks to reach from a to b by down arrow keys
  def calcDownClickCount(a: Int, b: Int): Int = {
    var steps = 0
    if (a <= b) {
      val aWrappedAround = a + (problemInstance.getMaxChannel() - problemInstance.getMinChannel() + 1)
      //For wraparound and the equal to is to simulate trying to get to the same channel via the down buttons
      steps = ((aWrappedAround - b) - problemInstance.getBlockedChannelCount(problemInstance.getMinChannel(), a) - problemInstance.getBlockedChannelCount(b,problemInstance.getMaxChannel()) )
    }
    else
      steps = (a - b) - problemInstance.getBlockedChannelCount(b, a)

    steps
  }

  //Calculates clicks to reach from a to b by up arrow keys
  def calcUpClickCount(start: Int, end: Int): Int = {
    var steps:Int =0
    if (start >= end) //For wraparound and the equal to is to simulate trying to get to the same channel via the up buttons.
    {
      val bWrappedAround = end + (problemInstance.getMaxChannel() - problemInstance.getMinChannel() + 1)
      steps = (bWrappedAround - start) - problemInstance.getBlockedChannelCount(start, problemInstance.getMaxChannel()) - problemInstance.getBlockedChannelCount(problemInstance.getMinChannel(), end)
    }
    else
      steps = (end - start) - problemInstance.getBlockedChannelCount(start, end)

    steps
  }


  def calcMinClicks: Int = {
    //Move to the first Channel to view by using the digit.
    //Assume, the channel obtained when switching on tv, is not the required channel to be viewed or is closed by any other combination

    //Calculates the minClicks required
    val pairs: Array[(Int, Int)] = problemInstance.getViewingChannelSequence zip problemInstance.getViewingChannelSequence.tail
    val minClicksBetweenPairs = pairs.map(t => calcMinClicksBetween(t._1, t._2))
    Utility.GetNumOfDigits(problemInstance.getViewingChannelSequence(curIndex)) + minClicksBetweenPairs.reduce((sofar,cur) => sofar + cur)
  }

  def calcMinClicksBetween(fromCh: Int, toCh: Int): Int = {

    //The cases are arranged in the order that can be preferrable for the next iteration.
    //Eg: In case of contention on the minimum clicks by two methods.(moving from 114 to 117).
    //Direct digit press 117 is better than using up arrows thrice, since the reachable scope with one button(back) increases.
    //The effect can be seen if the next digit is 113(114 - 116 - 113) requiring only 2(back and a down button) if, prev channel had been 114.

    //Case 1: Calculate no. of clicks on pressing the digits of the channel
    val digitClicks: Int = Utility.GetNumOfDigits(toCh)

    //Case 2: Calculate no. of clicks for going to prev channel(back button)
    //Case 3: Calculate no. of clicks for going to prev channel(back button) and using up/down clicks to proceed to the required channel
    val backAndUpDownCombinationClicks: Int = calcMinClicksWithBack(fromCh, toCh)

    //Case 4: Calculate no. of clicks for going from current to next channel using minChannel plus down channel keys
    val minChannelClickCount: Int = calcMinClicksWithMinChannel(fromCh, toCh)

    //Case 5: Calculate no. of clicks in going from current to next channel by using up/down arrow keys
    val upDownArrowClicks: Int = calcUpDownClickCount(fromCh, toCh)


    val minClicks: Int = Utility.minimum(digitClicks, backAndUpDownCombinationClicks, minChannelClickCount, upDownArrowClicks)

    //To Track what will be the prev Channel for the next iteration
    if (minClicks == digitClicks)
      prevChannel = fromCh
    else if (minClicks == backAndUpDownCombinationClicks)
      prevChannel = prevForNextIterationByBackChannelUpDownClick
    else if (minClicks == minChannelClickCount)
      prevChannel = prevForNextIterationByMinChannelDownClick
    else
      prevChannel = prevForNextIterationByUpDownClick

    return minClicks
  }
  
  def getPreviousChannelUp(start: Int, end: Int) = {
    if (start < end){
      (start to end).map(x => (x, problemInstance.isBlocked(x))).filterNot(_._2).last._1
    }else{
      ((start to problemInstance.getMaxChannel()) ++ (problemInstance.getMinChannel() to end))
        .map(x => (x, problemInstance.isBlocked(x)))
        .filterNot(_._2)
        .last
        ._1
    }
  }

  def getPreviousChannelDown(start: Int, end: Int) = {
    if (start > end){
      (end to start).map(x => (x, problemInstance.isBlocked(x))).filterNot(_._2).last._1
    }else{
      ((start to problemInstance.getMinChannel()).reverse ++ (problemInstance.getMaxChannel() to end by -1))
        .map(x => (x, problemInstance.isBlocked(x)))
        .filterNot(_._2)
        .last
        ._1
    }
  }

  //Calculates clicks to reach from a to b by up/down arrow keys
  def calcUpDownClickCount(start: Int, end: Int): Int = {

    val ups: Int = calcUpClickCount(start, end)
    val downs: Int = calcDownClickCount(start, end)
    if (ups < downs) {
      prevForNextIterationByUpDownClick = getPreviousChannelUp(start, end)
      ups
    }
    else {
      prevForNextIterationByUpDownClick = getPreviousChannelDown(start, end)
      downs
    }

  }

  def calcMinClicksWithBack(fromCh: Int, toCh: Int): Int = {

    var backAndUpDownCombinationClicks: Int = 999999; //arbitarily large number for the case of the calculation of the back button for the first channel
    if (curIndex >= 1) {
      backAndUpDownCombinationClicks = 1
      //Track potential previous Channel for next iteration
      prevForNextIterationByBackChannelUpDownClick = fromCh; //If just back is pressed, it is the current channel that becomes
      //the prev channel for next iteration
      if (!(prevChannel == toCh)) {
        backAndUpDownCombinationClicks += calcUpDownClickCount(prevChannel, toCh)
        prevForNextIterationByBackChannelUpDownClick = prevForNextIterationByUpDownClick;
        //If up/down combination is used with back key, update the
        //previous channel for next iteration from what was updated in up/down click counts method.
      }
    }
    return backAndUpDownCombinationClicks
  }

  //Case 4: Calculate clicks by moving to minimum channel and using wraparound (down key)
  //Note: If current channel itself is min, then the lower value will be obtained via calUpDownSteps. We can avoid extra check here whether current channel is min.
  //We dont need to go to the max channel and use the up arrow, since the digits of the lower channel will be less than equal to the max channel.
  def calcMinClicksWithMinChannel(fromCh: Int, toCh: Int): Int = {

    val minChannel: Int = problemInstance.getMinChannel()
    val m: Int = Utility.GetNumOfDigits(minChannel) + calcDownClickCount(minChannel, toCh)

    //Track previous channel for next iteration
    // TODO: Edge case where toCh is the end of the list

    prevForNextIterationByMinChannelDownClick = toCh + 1

//    while(problemInstance.isBlocked(prevForNextIterationByMinChannelDownClick) && !problemInstance.isWithinChannelLimits(prevForNextIterationByMinChannelDownClick)){
//      
//    }

    if(toCh == problemInstance.getMaxChannel())
      prevForNextIterationByMinChannelDownClick = problemInstance.getMinChannel()
    else {
        prevForNextIterationByMinChannelDownClick = toCh + 1
        while (problemInstance.isBlocked(prevForNextIterationByMinChannelDownClick))
        prevForNextIterationByMinChannelDownClick += 1
    }
    return m
  }

}
