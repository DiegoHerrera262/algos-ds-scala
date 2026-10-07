package algos

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class PairwiseProductTests extends munit.ScalaCheckSuite {
  test("Max Knockout Strategy works") {
    assertEquals(maxKnockout((-1L, -1L), 10L), (10L, -1L))
    assertEquals(maxKnockout((10L, 8L), 7L), (10L, 8L))
    assertEquals(maxKnockout((10L, 8L), 8L), (10L, 8L))
    assertEquals(maxKnockout((10L, 8L), 9L), (10L, 9L))
    assertEquals(maxKnockout((10L, 8L), 11L), (11L, 10L))
    assertEquals(maxKnockout((10L, 8L), 10L), (10L, 10L))
  }

  test("Naive solution passes the examples") {
    assertEquals(naiveMaxPairProduct(Array(1L, 2L, 3L)), 6L)
    assertEquals(
      naiveMaxPairProduct(Array(7L, 5L, 14L, 2L, 8L, 8L, 10L, 1L, 2L, 3L)),
      140L
    )
  }
  test("Refined solution passes the examples") {
    assertEquals(maxPairProduct(Array(1L, 2L, 3L)), 6L)
    assertEquals(
      maxPairProduct(Array(7L, 5L, 14L, 2L, 8L, 8L, 10L, 1L, 2L, 3L)),
      140L
    )
  }

  val comparativeSample: Gen[Array[Long]] = for {
    size <- Gen.choose(2, 10)
    list <- Gen.listOfN(size, Gen.choose(0L, 100L))
  } yield list.toArray

  property("Naive and refine solution match always") {
    forAll(comparativeSample) { (list: Array[Long]) =>
      assertEquals(naiveMaxPairProduct(list), maxPairProduct(list))
    }
  }

  val realWorldSample: Gen[Array[Long]] = for {
    size <- Gen.choose(10000, 200000)
    list <- Gen.listOfN(size, Gen.choose(0L, 100000L))
  } yield list.toArray

  property("Refined solution handles full input size in time") {
    forAll(realWorldSample) { (list: Array[Long]) =>
      assert(maxPairProduct(list) >= list.min * list.max)
    }
  }
}
