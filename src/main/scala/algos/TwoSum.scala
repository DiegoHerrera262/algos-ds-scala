package algos

import scala.io.StdIn.{readLine}

def sum(stringList: Array[String]): Int = {
  stringList
    .flatMap(_.toIntOption)
    .reduce((a, b) => a + b)
}

@main def TwoSum(args: String*): Unit = {
  println(sum(readLine().split(" ")))
}
