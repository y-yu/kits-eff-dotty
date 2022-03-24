package kits.eff

import kits.eff.TailRec.*
import scala.annotation.tailrec

sealed trait TailRec[A] {
  final def run: A = {
    @tailrec
    def loop(
      rest: TailRec[A],
      cont: A => TailRec[A]
    ): A = rest match {
      case Return(v)  =>
        loop(cont(v))
      case Suspend(k) =>
        loop(k(), cont)
      case FlatMap(x, f) =>
        x match {
          case Return(v)  =>
            loop(f(v), cont)
          case FlatMap(y, g) =>
            loop(y.flatMap(a => g(a).flatMap(f)), cont)
          //f(g(y.run).run).run
          case Suspend(k) =>
            loop(k(), a => loop(f(a), cont))
        }
    }
    loop(this, )
/*

    this match {
      case Return(v)  => v
      case Suspend(k) =>
        k().run
      //???
      case FlatMap(x, f) =>
        x match {
          case Return(v)  =>
            f(v).run
          case FlatMap(y, g) =>
            y.flatMap(a => g(a).flatMap(f)).run
          //f(g(y.run).run).run
          case Suspend(k) =>
            f(k().run).run
        }
    }
*/
  }

  def map[B](f: A => B): TailRec[B] =
    this.flatMap(a => Return(f(a)))

  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap(this, f)

}

object TailRec {
  case class Return[A](v: A) extends TailRec[A]
  case class Suspend[A](resume: () => TailRec[A]) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
}
