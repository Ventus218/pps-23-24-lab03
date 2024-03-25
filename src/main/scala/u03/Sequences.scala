package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u03.Optionals.Optional.*

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
      flatMap(l)(e => Cons(mapper(e), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(l1)(e => pred(e) match
        case true => Cons(e, Nil())
        case _ => Nil()
        )

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(firstH, firstT), Cons(secondH, secondT)) => Cons((firstH, secondH), zip(firstT, secondT))
      case _ => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case (Cons(h, t)) if n > 0 => Cons(h, take(t)(n - 1))
      case _ => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(h, t) => Cons(h, concat(t, l2))
      case Nil() => l2
        
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    def min(l: Sequence[Int]): Optional[Int] =
      def _min(l: Sequence[Int], minV: Optional[Int]): Optional[Int] = (l, minV) match
        case (Cons(h, t), Empty()) => _min(t, Just(h))
        case (Cons(h, t), Just(v)) => if h < v then _min(t, Just(h)) else _min(t, Just(v))
        case _ => minV

      _min(l, Empty())

@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
