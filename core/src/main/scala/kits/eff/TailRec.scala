package kits.eff

import kits.eff.TailRec.*
import scala.annotation.tailrec

sealed trait TailRec[+A] {
  @tailrec
  final def run : A = this match {
    case Return(v)  => v
    case Suspend(k) =>
      k().run
      /*
    case FlatMap(x, f) =>
      x match {
        case Return(v)  =>
          f(v).run
        case FlatMap(y, g) =>
          f(g(y.run).run).run
        case Suspend(k) =>
          k().run
          //f(k().run).run
          ???
      }
      */
  }

  def map[B](f: A => B): TailRec[B] =
    Return(f(run))
}

object TailRec {
  case class Return[A](v: A) extends TailRec[A]
  case class Suspend[A](resume: () => TailRec[A]) extends TailRec[A]
  //case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
}
