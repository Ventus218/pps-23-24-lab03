package task2

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.*
import u03.Sequences.Sequence.*
import task2.Es4.foldLeft

class Es4Tests:
    
    @Test def testWithNormalList =
        val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))
        assertEquals(-16, foldLeft(lst)(0)(_ - _))
        assertEquals(16, foldLeft(lst)(0)(_ + _))
    
    @Test def testWithEmptyList =
        val defaultValue = 0
        assertEquals(defaultValue, foldLeft(Nil[Int]())(defaultValue)(_ + _))
    
    @Test def testWithOneValueList =
        val lst = Cons(5, Nil())
        assertEquals(5, foldLeft(lst)(0)(_ + _))
    
    @Test def testGenericity =
        val lst = Cons(0, Cons(0, Nil()))
        assertEquals("..", foldLeft(lst)("")((d, _) => d concat "."))
