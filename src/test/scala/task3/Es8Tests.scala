package task3

import org.junit.Test
import org.junit.Assert.*
import u03.Streams.*
import u03.Sequences.Sequence.*
import task3.Es8.pellStream

class Es8Tests: 

    @Test def pell(): Unit =
        val pell: Stream[Int] = pellStream()
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))), Stream.toList(Stream.take(pell)(5)))
