/**
 * Created by andrewhe on 2015/07/17.
 */
object Application extends App {
  var inputInt = readLine().toCharArray

  while (!isPalindrome(inputInt.mkString)) {
    inputInt = incrementStringInteger(inputInt, 1)
    println(inputInt.mkString)
  }

  println(inputInt.mkString)

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
        val newLastDigit = x + 1
        number.updated(number.length - index, newLastDigit.toChar)
      case _ =>
        incrementStringInteger(number.updated(number.length - index, '0'), index + 1)
    }
  }
}
