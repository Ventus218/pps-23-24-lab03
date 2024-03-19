package task3

import u03.Streams.*

object Es8:

    private def pell(n: Int): Int = n match
        case 0 => 0
        case 1 => 1
        case _ => 2 * pell(n - 1) + pell(n - 2)

    def pellStream(): Stream[Int] =
        Stream.map(Stream.iterate(0)(_ + 1))(pell(_))
