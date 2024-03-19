package task2

import u03.Sequences.*
import u03.Sequences.Sequence.*

object Es5:

    extension [A] (s: Sequence[A])

        def map[B](mapper: A => B): Sequence[B] =
            Sequence.map(s)(mapper)

        def filter(pred: A => Boolean): Sequence[A] =
            Sequence.filter(s)(pred)

        // Lab 03
        def zip[B](second: Sequence[B]): Sequence[(A, B)] =
            Sequence.zip(s, second)

        def take(n: Int): Sequence[A] = 
            Sequence.take(s)(n)
        
        def concat(l2: Sequence[A]): Sequence[A] = 
            Sequence.concat(s, l2)
            
        def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = 
            Sequence.flatMap(s)(mapper)

        def foldLeft[R](d: R)(f: (R, A) => R): R =
            Es4.foldLeft(s)(d)(f)
