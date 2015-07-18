object Main extends App {
  val numberOfCases = readInt()
  var results: Array[String] = Array[String]()
  var x = 0

  for (x <- 0 until numberOfCases) {
    var inputInt = incrementStringInteger(readLine().toCharArray, 1)

    while (!isPalindrome(inputInt.mkString)) {
      inputInt = incrementStringInteger(inputInt, 1)
    }
    results = results :+ inputInt.mkString
  }

  results.foreach(println)

  def isPalindrome(number: String): Boolean = {
    if (number.length < 2)
      true
    else {
      val numberReverse = number.reverse
      if (number.equals(numberReverse)) {
        true
      }
      else
        false
    }
  }

  def incrementStringInteger(number: Array[Char], index: Int): Array[Char] = {
    val lastDigit = number.takeRight(index)(0)
    lastDigit.toInt match {
      case x if x < 57 =>
        if (number.length - index < 0)
          '1' +: number
        else {
          val newLastDigit = x + 1
          number.updated(number.length - index, newLastDigit.toChar)
        }
      case _ =>
        incrementStringInteger(number.updated(number.length - index, '0'), index + 1)
    }
  }
}
