package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*

class SequenceTest:
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
  
  @Test def testTake() = 
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(5))
    assertEquals(Nil(), take(Nil())(2))
  
  @Test def testZip(): Unit = 
    val list2 = Cons("a", Cons("b", Cons("c", Nil())))
    assertEquals(Cons((10, "a"), Cons((20, "b"), Cons((30, "c"), Nil()))), zip(l , list2))
    assertEquals(Nil(), zip(Nil(), Nil()))
    assertEquals(Nil(), zip(Nil(), l))
    assertEquals(Nil(), zip(l, Nil()))

  @Test def testConcat(): Unit = ???

  @Test def testFlatMap(): Unit = ???

  @Test def testMin(): Unit = ???
