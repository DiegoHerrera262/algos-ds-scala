package algos

import scala.io.StdIn.{readLine}

def naiveMaxPairProduct(numberList: Array[Long]) = {
  val allProducts = for(
    (x, idx) <- numberList.zipWithIndex;
    (y, jdx) <- numberList.zipWithIndex
    if idx < jdx
  ) yield x * y
  allProducts.max
}

def maxKnockout(maxVals: (Long, Long), number: Long): (Long, Long) = {
  val (maxVal, secondMaxVal) = maxVals
  if (number < secondMaxVal) return maxVals
  else if (number < maxVal) return (maxVal, number)
  else return (number, maxVal)
}

def maxPairProduct(numberList: Array[Long]): Long = {
  numberList
    .foldLeft((-1L, -1L))((maxVals, number) => maxKnockout(maxVals, number))
    .toList
    .reduce(_ * _)
}

@main def PairwiseProduct(args: String*): Unit = {
  val arraySize = readLine()
  val numberList = readLine().split(" ")
  println(maxPairProduct(numberList.flatMap(_.toLongOption)))
}
