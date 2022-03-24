package kits.eff

import scala.annotation.tailrec

enum Arrs[+R[_], A, B] extends (A => Eff[R, B]) {
  case Leaf[+R[_], A, B](arr: A => Eff[R, B]) extends Arrs[R, A, B]

  case Node[+R[_], A, B, C](left: Arrs[R, A, B], right: Arrs[R, B, C]) extends Arrs[R, A, C]

  private def applyInternal(a: A): TailRec[Eff[R, B]] = {
    def goAux[R[_], A, B, C](
      arrs: Arrs[R, A, C],
      k: (A => TailRec[Eff[R, C]]) => TailRec[Eff[R, B]]
    ): TailRec[Eff[R, B]] = go(arrs, k)

    /*
    def goNode[A, B, C](
      left: Arrs[R, A, C],
      right: Arrs[R, C, B],
      k: (A => Eff[R, C]) => TailRec[Eff[R, B]]
    ): TailRec[Eff[R, B]] =
      left.view match {
        case Arrs.View.One(f: (A => Eff[R, C])) =>
          go(right, g => k(a => f(a))

        case Arrs.View.Cons(f: (A => Eff[R, b]), g) =>
      }
    */

    @tailrec
    def go[R[_], A, B, C](
      arrs: Arrs[R, A, C],
      k: (A => TailRec[Eff[R, C]]) => TailRec[Eff[R, B]]
    ): TailRec[Eff[R, B]] =
      arrs.view match {
        case Arrs.View.One(f: (A => Eff[R, B])) =>
          k(a => TailRec.Return(f(a)))
          //k(a => f(a))
        case Arrs.View.Cons(f: (A => Eff[R, b]), g) =>
          go(
            g.asInstanceOf[Arrs[R, b, C]],
            (right: (b => TailRec[Eff[R, C]])) =>
              TailRec.Suspend(() =>
                goAux(
                  f.asInstanceOf[Arrs[R, A, b]],
                  (left: (A => TailRec[Eff[R, b]])) =>
                    //TailRec.Suspend(() =>
                      k(a =>
                        left(a).flatMap {
                          case Eff.Pure(b) =>
                            right(b)
                          case Eff.Impure(r, h) =>
                            TailRec.Return(Eff.Impure(r, h ++ g.asInstanceOf[Arrs[R, b, C]]))
                        }
                      )
                    //)
                )
              )
          )
          /*
          go(
            f.asInstanceOf[Arrs[R, A, b]],
            (left: (A => Eff[R, b])) =>
              TailRec.Suspend(() =>
                goAux(
                  g.asInstanceOf[Arrs[R, b, C]],
                  (right: (b => Eff[R, C])) =>
                    k((a: A) =>
                      //TailRec.Suspend(() =>
                        left(a) match {
                          case Eff.Pure(b) =>
                            right(b)
                          case Eff.Impure(r, h) =>
                            Eff.Impure(r, h ++ g.asInstanceOf[Arrs[R, b, C]])
                        }
                      //)
                    )
                )
              )
          )
          */

          /*
          go(
            g.asInstanceOf[Arrs[R, b, C]],
            (right: ((b => TailRec[Eff[R, C]]))) =>
              TailRec.Suspend(() =>
                k(a =>
                  TailRec.Suspend(() =>
                    f(a) match {
                      case Eff.Pure(b) =>
                        right(b)
                      case Eff.Impure(r, h) =>
                        TailRec.Return(Eff.Impure(r, h ++ g))
                    }
                  )
                )
              )
          )
          */
          /*
          go(
            f.asInstanceOf[Arrs[R, A, b]],
            (left: ((A => TailRec[Eff[R, b]]))) =>
              TailRec.Suspend(() =>
                k(a =>
                  left(a) match {
                    case Eff.Pure(b) =>
                      //g.asInstanceOf[Arrs[R, b, C]].applyInternal(b)
                      ???
                    case Eff.Impure(r, h) =>
                      Eff.Impure(r, h ++ g.asInstanceOf[Arrs[R, b, C]])
                  }
                )
              )
          )
          */
          /*
          go(
            g.asInstanceOf[Arrs[R, b, C]],
            (right: (b => Eff[R, C])) =>
              go(
                f.asInstanceOf[Arrs[R, A, b]],
                (left: ((A => Eff[R, b]))) =>
                  TailRec.Suspend(() =>
                    k((a: A) =>
                      left(a) match {
                        case Eff.Pure(b) =>
                          right(b)
                        case Eff.Impure(r, h) =>
                          Eff.Impure(r, h ++ g.asInstanceOf[Arrs[R, b, C]])
                      }
                    )
                  )
              )
          )*/
    }

    go(this, f => f(a))
    //go(this, f => TailRec.Return(f(a)))
    //???
  }

  def apply(a: A): Eff[R, B] = {
    applyInternal(a).run
  }

  /*
  def view: Arrs.View[R, A, B] = {
    @tailrec
    def loop[A, C](
      head: Arrs[R, A, C],
      k: Arrs.View[R, A, C] => TailRec[Arrs.View[R, A, B]]
    ): TailRec[Arrs.View[R, A, B]] =
      head match {
        case Leaf(f: (A => Eff[R, C])) =>
          k(Arrs.View.One(f))
        case Node(l: Arrs[R, A, b], r /* Arrs[R, b, C] */) =>
          loop(
            r,
            (right: Arrs.View[R, b, C]) =>
              loop(
                l,
                (left: Arrs.View[R, A, b]) =>
                  Arrs.View.FlatMap(
                    left,
                    (b: b) =>
                      b
                  )
              )
          )
      }
    this match {
      case Leaf(f) => Arrs.View.One(f)
      case Node(l, r) =>
        //loop()
        ???
    }
    ???
  }
  */

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
