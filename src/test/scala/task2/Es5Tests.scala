package task2

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.*
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence.Nil
import task2.Es5.* 

class Es5Tests:

    val s: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

    @Test def testMap() =
        val mapper = (_: Int) => "."
        assertEquals(Sequence.map(s)(mapper), s.map(mapper))
    
    @Test def testFilter() =
        val pred = (_: Int) >= 20
        assertEquals(Sequence.filter(s)(pred), s.filter(pred))
    
    @Test def testZip() =
        val s2 = Cons(".", Cons(".", Cons(".", Nil())))
        assertEquals(Sequence.zip(s, s2), s.zip(s2))
    
    @Test def testTake() =
        assertEquals(Sequence.take(s)(2), s.take(2))
    
    @Test def testConcat() =
        val s2 = Cons(40, Cons(50, Cons(60, Nil())))
        assertEquals(Sequence.concat(s, s2), s.concat(s2))
    
    @Test def testFlatMap() =
        val mapper = (_: Int) => Cons(".", Cons(".", Nil()))
        assertEquals(Sequence.flatMap(s)(mapper), s.flatMap(mapper))

    @Test def testFoldLeft() =
        val f: (Int, Int) => Int = _ + _
        assertEquals(Es4.foldLeft(s)(0)(f), s.foldLeft(0)(f))
