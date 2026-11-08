package algos

import scala.annotation.tailrec

def recursiveFibonacci(n: Int): BigInt = {
  if (n <= 0) {
    return 0;
  } else if (n == 1) {
    return 1;
  } else {
    return recursiveFibonacci(n - 2) + recursiveFibonacci(n - 1);
  }
}

def fibonacciRule(current: (BigInt, BigInt)): (BigInt, BigInt) = {
  (current._2, current._1 + current._2)
}

def iterativeFibonacci(n: Int): BigInt = {
  return (0 to n)
    .foldLeft((BigInt(-1), BigInt(1)))((acc, x) => fibonacciRule(acc))
    ._2;
}

@tailrec
def fibonacciIteration(
    current: (BigInt, BigInt),
    idx: Int,
    n: Int
): (BigInt, BigInt) = {
  if (idx == n) {
    return current;
  } else {
    return fibonacciIteration(
      (current._2, current._1 + current._2),
      idx + 1,
      n
    );
  }
}

/** This function should be optimized since scala has tail call optimization */
def fibonacci(n: Int): BigInt = {
  return fibonacciIteration((BigInt(0), BigInt(1)), 0, n)._1;
}
