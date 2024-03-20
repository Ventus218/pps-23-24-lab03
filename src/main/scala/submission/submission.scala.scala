package submission

// Task 1
//  Es1
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

def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
    flatMap(l)(e => Cons(mapper(e), Nil()))

def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
    flatMap(l1)(e => pred(e) match
    case true => Cons(e, Nil())
    case _ => Nil()
    )

//  Es2
def min(l: Sequence[Int]): Optional[Int] =
    def _min(l: Sequence[Int], minV: Int): Int = (l, minV) match
    // Si potrebbe usare una funzione di libreria per il minimo tra h e v
    // e questo eliminerebbe un if ma non so se era possibile farlo per l'esercizio
    case (Cons(h, t), v) if h < v => _min(t, h)
    case (Cons(h, t), v) if h >= v => _min(t, v)
    case _ => minV

    l match
    case Cons(h, t) => Optional.Just(_min(t, h))
    case _ => Optional.Empty()


// Task 2
object Es3:
    def mapToCourses(s: Sequence[Person]): Sequence[String] = 
        flatMap(s)(_ match
            case Teacher(name, course) => Cons(course, Nil())
            case _ => Nil()
        )

object Es4:
    def foldLeft[R, T](s: Sequence[T])(d: R)(f: (R, T) => R): R = s match
        case Cons(h, t) => foldLeft(t)(f(d, h))(f)
        case _ => d

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


// Task 3
//  Es6
// slides ask for a different takeWhile but i think that's an error because of the given example:
// "Define a function, called takeWhile(s)(n), that returns the first
// n elements of the stream s that satisfy a given predicate."
def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
    case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
    case _ => empty()

//  Es7
def fill[A](n: Int)(filler: A): Stream[A] = n match
    case n if n > 0 => cons(filler, fill(n - 1)(filler))
    case _ => empty()

//  Es8
object Es8:
    private def pell(n: Int): Int = n match
        case 0 => 0
        case 1 => 1
        case _ => 2 * pell(n - 1) + pell(n - 2)

    def pellStream(): Stream[Int] =
        Stream.map(Stream.iterate(0)(_ + 1))(pell(_))