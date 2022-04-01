package u05lab.ex1

import org.junit.Assert.*
import org.junit.Test

class ListTest {

  val reference: List[Int] = List(1, 2, 3, 4)

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1,0),(2,1),(3,2),(4,3)), reference.zipRight)

  @Test
  def testPartition(): Unit =
    assertEquals((List(2,4), List(1,3)), reference.partition(_ % 2 == 0))

  @Test
  def testReverse(): Unit =
    assertEquals(List(4,3,2,1), reference.reverse())

  @Test
  def testTakeRight(): Unit =
    assertEquals(List(2,3,4), reference.takeRight(3))

  @Test
  def testTakeLeft(): Unit =
    assertEquals(List(1,2,3), reference.takeLeft(3))

  @Test
  def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), reference.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), reference.span(_ < 3))

  @Test
  def testReduce(): Unit =
    assertEquals(10, reference.reduce(_ + _))
    assertThrows(classOf[UnsupportedOperationException], () => Nil.reduce[Int](_ + _))

  @Test
  def testCollect(): Unit =
    assertEquals(List(4,6,8), reference.collect({ case x if x >= 2 => x*2}))
}
