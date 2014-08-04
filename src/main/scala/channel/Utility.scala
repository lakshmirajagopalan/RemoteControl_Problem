package channel {

object Utility {

  def GetNumOfDigits(digit: Int): Int = {
    var n: Int = 0
    var digitVar = digit
    while (digitVar != 0) {
      n += 1
      digitVar = digitVar / 10
    }
    return n
  }

  def minimum(a: Int, b: Int, c: Int, d: Int): Int = {
    var min1: Int = a
    var min2: Int = c

    if (b < a)
      min1 = b
    if (d < c)
      min2 = d

    if (min1 <= min2) min1
    else min2
  }

  def swap(a: Int, b: Int): Tuple2[Int, Int] = {
    (b, a)
  }
}

}