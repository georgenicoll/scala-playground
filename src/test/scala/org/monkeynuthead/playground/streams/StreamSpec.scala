package org.monkeynuthead.playground.streams

import org.scalatest.{MustMatchers, WordSpec}

/**
 * Messing around with streams.
 */
class StreamSpec extends WordSpec with MustMatchers {

  "I" must {

    "know how to create a basic stream (but be careful because the val will keep hold of everything)" in {

      val ints: Stream[Int] = {
        def loop(i: Int): Stream[Int] = i #:: loop(i + 1)
        loop(10)
      }

      ints.take(3) must equal(List(10, 11, 12))
      ints.head must equal(10)
      val (head1, ints1) = (ints.head, ints.tail)
      head1 must equal(10)
      val (head2, ints2) = (ints1.head, ints1.tail)
      head2 must equal(11)
      ints2.head must equal(12)
    }

    "know how to create a basic stream (but not holding on to head using a def)" in {

      def ints: Stream[Int] = {
        def loop(i: Int): Stream[Int] = i #:: loop(i + 1)
        loop(5)
      }

      val it = ints.iterator
      it.map(2*).take(5).toSeq must equal(List(10,12,14,16,18))

    }

    "know how to do something slightly more complicated" in {

      def repeated[A](underlying: IndexedSeq[A]): Stream[A] = {
        def loop(i: Int, underlying: IndexedSeq[A]): Stream[A] = {
          val index = i % underlying.length
          underlying(index) #:: loop(index + 1, underlying)
        }
        loop(0, underlying)
      }

      val it = repeated(Vector("A", "B", "C")).iterator
      it.take(10).toSeq must equal(List("A", "B", "C", "A", "B", "C", "A", "B", "C", "A"))

    }

  }

}
