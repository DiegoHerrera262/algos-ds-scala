package algos

import algos.sum

class TwoSumTests extends munit.FunSuite {
  test("Sum of intergers in a string list") {
    assertEquals(sum(Array("1", "2", "3", "4", "5")), 15)
  }
  test("Handles negative and zero values") {
    assertEquals(sum(Array("-1", "0", "1")), 0)
  }
  test("Hanldes gracefully wrong inputs by omitting them") {
    assertEquals(sum(Array("orange", "21", "apple", "12")), 33)
  }
}
