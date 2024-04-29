// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class BVecSuite extends munit.FunSuite {
  test("BVec empty on create") {
    val empty = BVec.empty[Int]
    assertEquals(empty.size(), 0)
  }

  test("BVec add/get") {
    val empty = BVec.empty[Int]
    val v1 = empty :+ 7
    assertEquals(v1.size(), 1)
    val v2 = v1 :+ 9
    assertEquals(v2.size(), 2)
    assertEquals(v1.size(), 1)
    assertEquals(v2(0), 7)
    assertEquals(v2(1), 9)
  }

  test("BVec add/set") {
    val empty = BVec.empty[Int]
    val v1 = empty :+ 7
    assertEquals(v1.size(), 1)
    val v2 = v1 :+ 9
    assertEquals(v2.size(), 2)
    assertEquals(v1.size(), 1)
    val v3 = v2.updated(0, 99)
    assertEquals(v3.size(), 2)
    assertEquals(v1.size(), 1)
    assertEquals(v3(0), 99)
    assertEquals(v3(1), 9)
    assertEquals(v2(0), 7)
  }

  test("Big add") {
    val empty = BVec.empty[Int]
    val v = empty :+ 1 :+ 2 :+ 3 :+ 4 :+ 5 :+ 6
    assertEquals(v(0), 1)
    assertEquals(v(1), 2)
    assertEquals(v(2), 3)
    assertEquals(v(3), 4)
    assertEquals(v(4), 5)
    assertEquals(v(5), 6)
    assertEquals(v.size(), 6)
    val v2 = v.updated(2, 99)
    assertEquals(v(2), 3)
    assertEquals(v2(2), 99)
  }

  test("Big test") {
    var v = BVec.empty[Int]
    for (i <- 1 to 1000) {
      v = v :+ i
      assertEquals(v.size(), i)
    }
    for (i <- 1 to 1000) {
      assertEquals(v(i-1), i)
    }
  }
}
