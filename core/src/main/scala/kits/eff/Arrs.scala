package kits.eff

import scala.annotation.tailrec

enum Arrs[+R[_], A, B] extends (A => Eff[R, B]) {
  case Leaf[+R[_], A, B](arr: A => Eff[R, B]) extends Arrs[R, A, B]

  case Node[+R[_], A, B, C](left: Arrs[R, A, B], right: Arrs[R, B, C]) extends Arrs[R, A, C]

  private def applyInternal(a: A): TailRec[Eff[R, B]] = {
    def go[R[_], A, B](
      arrs: Arrs[R, A, B],
      k: (A => TailRec[Eff[R, B]]) => TailRec[Eff[R, B]]
    ): TailRec[Eff[R, B]] =
      arrs.view match {
        case Arrs.View.One(f) =>
          k(a => TailRec.Return(f(a)))
        case Arrs.View.Cons(f: (A => Eff[R, b]), g) =>
          go[R, b, B](
            g.asInstanceOf[Arrs[R, b, B]],
            x =>
                k((a: A) =>
                  TailRec.Suspend( () =>
                    go(f.asInstanceOf[Arrs[R, A, b]], y => y(a)).map[Eff[R, B]] {
                      {
                        case Eff.Pure(b) =>
                          x(b).run
                        case Eff.Impure(r, h) =>
                          Eff.Impure(r, h ++ g.asInstanceOf[Arrs[R, b, B]])
                      }
                    }
                  )
                  /*
                  TailRec.FlatMap(
                    go(f.asInstanceOf[Arrs[R, A, b]], y => y(a)),
                    {
                      case Eff.Pure(b) =>
                        x(b.asInstanceOf[b])
                      case Eff.Impure(r, h) =>
                        TailRec.Return(Eff.Impure(r, h ++ g.asInstanceOf[Arrs[R, b, B]]))
                    }
                  )
                  */
                )
                //f.go[R, A, b](f.asInstanceOf[Arrs[R, A, b]], y => TailRec.Return(y(a)))
                //???
                /*
                f(a) match {
                  case Eff.Pure(b) =>
                    x(b)
                  case Eff.Impure(r, h) =>
                    Eff.Impure(r, h ++ g.asInstanceOf[Arrs[R, b, B]])
                }
                */
          )

          /*
          val fa: Eff[R, b] = go[R, A, b](f.asInstanceOf[Arrs[R, A, b]], a, f => TailRec.Return(f)).run
          fa match {
            case Eff.Pure(b) =>
              go(
                g.asInstanceOf[Arrs[R, b, B]],
                b,
                x => {
                  TailRec.Suspend( () => k(x))
                }
              )
            case Eff.Impure(r, h) =>
              TailRec.Return(Eff.Impure(r, h ++ g.asInstanceOf[Arrs[R, b, B]]))
          }
          */
      }

    go(this, f => f(a))
  }

  def apply(a: A): Eff[R, B] = {
    applyInternal(a).run
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
