package algos

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class FibonacciTests extends munit.ScalaCheckSuite {
  test("recursiveFibonacci behaves as expected") {
    val examples = List(
      (0, 0),
      (1, 1),
      (2, 1),
      (3, 2),
      (4, 3),
      (5, 5),
      (6, 8),
      (7, 13),
      (8, 21),
      (9, 34),
      (10, 55)
    )
    examples.foreach { case (idx, fib) =>
      assertEquals(BigInt(fib), recursiveFibonacci(idx))
      assertEquals(BigInt(fib), iterativeFibonacci(idx))
      assertEquals(BigInt(fib), fibonacci(idx))
    }
  }

  val positiveInteger: Gen[Int] = Gen.choose(0, 20)

  property("Naive and efficient solution are equivalent") {
    forAll(positiveInteger) { n =>
      assertEquals(recursiveFibonacci(n), iterativeFibonacci(n))
      assertEquals(recursiveFibonacci(n), fibonacci(n))
    }
  }
}
