package task2

import task2.Es3.mapToCourses
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u02.Modules.Person
import u02.Modules.Person.*
import org.junit.Test
import org.junit.Assert.*

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
