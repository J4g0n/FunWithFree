def isPalindrome(n: Long): Boolean = {
  val strNum = n.toString
  val zippedList = strNum zip strNum.reverse
  zippedList.foldLeft (true) {
    (acc, x) =>
      x._1 == x._2 && acc
  }
}

val result = for {
  x <- Range(100, 999)
  y <- Range(x, 999)
  if isPalindrome(x * y)
} yield x * y

result
result.length
result.toSet.size
result.max