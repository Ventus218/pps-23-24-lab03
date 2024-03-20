package submission

class SequenceTest:
    import u03.Sequences.*
    import Sequence.*
    import Optionals.*

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

    @Test def testZip() = 
        val list2 = Cons("a", Cons("b", Cons("c", Nil())))
        assertEquals(Cons((10, "a"), Cons((20, "b"), Cons((30, "c"), Nil()))), zip(l , list2))
        assertEquals(Nil(), zip(Nil(), Nil()))
        assertEquals(Nil(), zip(Nil(), l))
        assertEquals(Nil(), zip(l, Nil()))

    @Test def testConcat() =
        val l1 = Cons(10, Cons(20, Nil()))
        val l2 = Cons(30, Cons(40, Nil()))
        assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), concat(l1 , l2))
        assertEquals(Cons(10, Cons(20, Nil())), concat(l1 , Nil()))
        assertEquals(Cons(30, Cons(40, Nil())), concat(Nil() , l2))


    @Test def testFlatMap() =
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
        assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))
        assertEquals(Nil(), flatMap(l)(v => Nil()))

    @Test def testMin() =
        assertEquals(Just(10), min(Cons(10, Cons(25, Cons(20, Nil())))))
        assertEquals(Just(5), min(Cons(10, Cons(5, Cons(20, Nil())))))
        assertEquals(Empty(), min(Nil()))


class Es3Tests:
    val stud1 = Student("Ale", 2019)
    val stud2 = Student("Ciccio", 2020)
    val teach1 = Teacher("Bravetti", "LCMC")
    val teach2 = Teacher("Ricci", "PCD")

    @Test def testMapEmptySequenceToCourses() =
        val emptySeq = Nil[Person]()
        assertEquals(Nil(), mapToCourses(emptySeq))
        
    @Test def testMapStudentSequenceToCourses() =
        val studentSeq = Cons(stud1, Cons(stud2, Nil()))
        assertEquals(Nil(), mapToCourses(studentSeq))
        
    @Test def testMapTeacherSequenceToCourses() =
        val teacherSeq = Cons(teach1, Cons(teach2, Nil()))
        assertEquals(Cons("LCMC", Cons("PCD", Nil())), mapToCourses(teacherSeq))

    @Test def testMapMixedPersonSequenceToCourses() =
        val seq = Cons(stud1, Cons(teach1, Cons(stud2, Cons(teach2, Nil()))))
        assertEquals(Cons("LCMC", Cons("PCD", Nil())), mapToCourses(seq))


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


// Es6 Test
@Test def takeWhile(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))
    
// Es7 Test
@Test def fill(): Unit =
    val filler = "a"
    assertEquals(Cons(filler, Cons(filler, Cons(filler, Nil()))), Stream.toList(Stream.fill(3)(filler)))
    assertEquals(Nil(), Stream.toList(Stream.fill(0)(filler)))


class Es8Tests: 
    @Test def pell(): Unit =
        val pell: Stream[Int] = pellStream()
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))), Stream.toList(Stream.take(pell)(5)))
