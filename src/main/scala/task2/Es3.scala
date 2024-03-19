package task2

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u02.Modules.Person
import u02.Modules.Person.*

object Es3:

    def mapToCourses(s: Sequence[Person]): Sequence[String] = 
        flatMap(s)(_ match
            case Teacher(name, course) => Cons(course, Nil())
            case _ => Nil()
        )
