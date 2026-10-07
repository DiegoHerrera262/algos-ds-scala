package algos

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class JosephusTests extends munit.ScalaCheckSuite {
  test("modularIndex handles circular indexing correctly when idx < n") {
    for (idx <- 1 to 8) {
      assertEquals(modularIndex(idx, 8), idx)
    }
  }
  test("modularIndex handles circular indexing correctly when idx >= n") {
    for ((idx, jdx) <- (9 to 16).zip(1 to 8)) {
      assertEquals(modularIndex(idx, 8), jdx)
    }
  }

  test("killAtIndex handles the process when index between boundaries") {
    val baseArray = Array(1, 2, 3, 4, 5)
    val examples = List(
      (Array(2, 3, 4, 5), 1),
      (Array(1, 3, 4, 5), 2),
      (Array(1, 2, 4, 5), 3),
      (Array(1, 2, 3, 5), 4),
      (Array(1, 2, 3, 4), 5)
    )
    examples.foreach { case (expected, idxToKill) =>
      assertEquals(expected.toSeq, killAtIndex(idxToKill, baseArray).toSeq)
    }
  }

  test("killAtIndex handles the process when index outside boundaries") {
    val baseArray = Array(1, 2, 3, 4, 5)
    val examples = List(
      (Array(2, 3, 4, 5), 6),
      (Array(1, 3, 4, 5), 7),
      (Array(1, 2, 4, 5), 8),
      (Array(1, 2, 3, 5), 9),
      (Array(1, 2, 3, 4), 10)
    )
    examples.foreach { case (expected, idxToKill) =>
      assertEquals(expected.toSeq, killAtIndex(idxToKill, baseArray).toSeq)
    }
  }

  test("""With a fully worked example, with k = 5 < 7 = n
          n = 7, [0 1 2 3 4 5 6]
          n = 6, [0 1 2 3 5 6]
          n = 5, [0 1 3 5 6]
          n = 4, [0 3 5 6]
          n = 3, [0 5 6]
          n = 2, [0 5]
          n = 1, [5]""") {
    val examples = List(
      (3, 5, 6),
      (2, 3, 5),
      (2, 2, 4),
      (3, 2, 3),
      (1, 3, 2)
    )
    examples.foreach { case (expectedNext, previousIndex, survivors) =>
      assertEquals(nextToEliminate(previousIndex, survivors, 5), expectedNext)
    }
  }

  test("Validate reindexing mapping") {
    val examples = List(
      (
        Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
        Array(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1, 2),
        18,
        4
      ),
      (
        Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        16,
        4
      ),
      (
        Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
        Array(4, 5, 6, 7, 8, 9, 10, 11, 1, 2, 3),
        13,
        5
      ),
      (
        Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
        Array(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1),
        16,
        3
      ),
      (
        Array(1, 2, 3),
        Array(2, 3, 1),
        4,
        3
      )
    )
    examples.foreach { case (originalSpace, reIndexedSpace, n, k) =>
      assertEquals(
        reIndexedSpace.map(idx => reindexing(idx, n, k)).toSeq,
        originalSpace.toSeq
      )
    }
  }

  test("Validate solution mapping") {
    val examples = List(
      (
        Array(1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15),
        Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        4
      ),
      (
        Array(1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15, 17, 18),
        Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
        4
      ),
      (
        Array(1, 2, 4, 5, 7, 8),
        Array(1, 2, 3, 4, 5, 6),
        3
      ),
      (
        Array(1, 2, 3, 4, 6, 7, 8, 9),
        Array(1, 2, 3, 4, 5, 6, 7, 8),
        5
      ),
      (
        Array(1, 3, 5, 7, 9),
        Array(1, 2, 3, 4, 5),
        2
      ),
      (
        Array(1, 2, 4),
        Array(1, 2, 3),
        3
      )
    )
    examples.foreach { case (originalSpace, reIndexedSpace, k) =>
      assertEquals(
        reIndexedSpace.map(idx => solutionMapping(idx, k)).toSeq,
        originalSpace.toSeq
      )
    }
  }

  test(
    "Comparing naive and efficient implementations of Josephus problem solution"
  ) {
    val examples = List(
      (4, 7, 3),
      (1, 4, 3),
      (1, 16, 2),
      (5, 15, 7),
      (10, 11, 19),
      (1, 1, 300),
      (11, 14, 5),
      (7, 7, 1)
    );
    examples.foreach { case (solution, n, k) =>
      assertEquals(solution, naiveJosephus(n, k))
      assertEquals(solution, iterativeJosephus(n, k))
      assertEquals(solution, josephus(n, k))
    }
  }

  val positiveInteger: Gen[Int] = Gen.choose(1, 500)
  val greaterThan2Int: Gen[Int] = Gen.choose(2, 500)

  property("Naive and efficient solutions are equivalent") {
    forAll(positiveInteger, greaterThan2Int) { (n: Int, k: Int) =>
      assertEquals(naiveJosephus(n, k), josephus(n, k))
    }
  }
}
