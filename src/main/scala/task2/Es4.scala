package task2

import u03.Sequences.*
import u03.Sequences.Sequence.*

object Es4:
    def foldLeft[R, T](s: Sequence[T])(d: R)(f: (R, T) => R): R = s match
        case Cons(h, t) => foldLeft(t)(f(d, h))(f)
        case _ => d
