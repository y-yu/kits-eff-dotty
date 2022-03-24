package kits.eff

import scala.annotation.tailrec
import scala.util.control.TailCalls.*

enum Arrs[+R[_], A, B] extends (A => Eff[R, B]) {
  case Leaf[+R[_], A, B](arr: A => Eff[R, B]) extends Arrs[R, A, B]

  case Node[+R[_], A, B, C](left: Arrs[R, A, B], right: Arrs[R, B, C]) extends Arrs[R, A, C]

  def apply(a: A): Eff[R, B] = {
    def go[A, B, C](
      arrs: Arrs[R, A, C],
      k: (A => TailRec[Eff[R, C]]) => TailRec[Eff[R, B]]
    ): TailRec[Eff[R, B]] =
      arrs match {
        case Leaf(f) =>
          k(a => done(f(a)))
        case Node(f: Arrs[R, A, b], g) =>
          go(
            f,
            (left: (A => TailRec[Eff[R, b]])) =>
              tailcall(
                go(
                  g,
                  (right: (b => TailRec[Eff[R, C]])) =>
                    k((a: A) =>
                      tailcall(
                        left(a).flatMap {
                          case Eff.Pure(b) =>
                            right(b)
                          case Eff.Impure(r, h) =>
                            done(Eff.Impure(r, h ++ g))
                        }
                      )
                    )
                )
              )
          )
    }

    go(this, f => f(a)).result
  }

  def view: Arrs.View[R, A, B] =
    this match {
      case Leaf(f) => Arrs.View.One(f)
      case Node(l, r) => Arrs.View.Cons(l, r)
    }

  def :+[S[_], C](f: B => Eff[S, C]): Arrs[[A] =>> R[A] | S[A], A, C] = Arrs.Node(this, Arrs.Leaf(f))

  def ++[S[_], C](f: Arrs[S, B, C]): Arrs[[A] =>> R[A] | S[A], A, C] = Arrs.Node(this, f)
}

object Arrs {
  enum View[+R[_], A, B] {
    case One[+R[_], A, B](arr: A => Eff[R, B]) extends View[R, A, B]
    case Cons[+R[_], A, B, C](arr: Arrs[R, A, B], arrs: Arrs[R, B, C]) extends View[R, A, C]
  }
}
