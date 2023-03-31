package kits.eff

import scala.quoted.*
import izumi.reflect.TagK
import izumi.reflect.macrortti.LightTypeTag
import scala.annotation.tailrec

case class Union[+R[_], A](tag: LightTypeTag, value: R[A]) {
  def decomp[F[_], S[_]](implicit
    F: TagK[F],
    ev: R[A] <:< (S[A] | F[A])
  ): Either[Union[S, A], F[A]] =
    if (tag <:< F.tag)
      Right(value.asInstanceOf[F[A]])
    else
      Left(this.asInstanceOf[Union[S, A]])
}

enum Eff[+R[_], A] {
  case Pure[+R[_], A](value: A) extends Eff[R, A]

  case Impure[+R[_], A, B](union: Union[R, A], arrs: Arrs[R, A, B]) extends Eff[R, B]

  def map[B](f: A => B): Eff[R, B] = flatMap(a => Pure(f(a)))

  def flatMap[S[_], B](f: A => Eff[S, B]): Eff[[A] =>> R[A] | S[A], B] =
    this match {
      case Pure(a) => f(a)
      case Impure(union: Union[R, a], arrs) => Impure[[A] =>> R[A] | S[A], a, B](union, arrs :+ f)
    }
}

object Eff {
  def apply[F[_], A](fa: F[A])(
    implicit F: TagK[F]
  ): Eff[F, A] = Impure(Union(F.tag, fa), Arrs.Leaf((a: A) => Pure(a)))

  trait Handler[F[_], R[_], A, B] {
    def pure(a: A): Eff[R, B]
    def flatMap[T](fa: F[T])(f: T => Eff[R, B]): Eff[R, B]
  }

  def handleRelay[F[_], R[_], A, B](eff: Eff[[X] =>> F[X] | R[X], A])(handler: Handler[F, R, A, B])(
    implicit F: TagK[F]
  ): Eff[R, B] =
    eff match {
      case Eff.Pure(a) => handler.pure(a)
      case Eff.Impure(u: Union[[X] =>> F[X] | R[X], a], k) =>
        u.decomp[F, R] match {
          case Right(fa) => handler.flatMap(fa)(x =>
            handleRelay[F, R, A, B](
              k(x)
            )(handler)
          )

          case Left(r) => Eff.Impure(r.asInstanceOf[Union[R, A]], Arrs.Leaf((a: A) =>
            handleRelay[F, R, A, B](
              k.asInstanceOf[A => Eff[[X] =>> F[X] | R[X], A]](a)
            )(handler))
          )
        }
    }

  trait HandlerS[F[_], R[_], S, A, B] {
    def pure(s: S, a: A): Eff[R, B]
    def flatMap[T](s: S, fa: F[T])(f: (S, T) => Eff[R, B]): Eff[R, B]
  }

  def handleRelayS[F[_], R[_], S, A, B](state: S, eff: Eff[[A] =>> F[A] | R[A], A])(handler: HandlerS[F, R, S, A, B])(
    implicit F: TagK[F]
  ): Eff[R, B] =
    eff match {
      case Eff.Pure(a) => handler.pure(state, a)
      case Eff.Impure(u: Union[[X] =>> F[X] | R[X], a], k) =>
        u.decomp[F, R] match {
          case Right(fa) => handler.flatMap(state, fa)((s, x) => handleRelayS[F, R, S, A, B](s, k(x))(handler))
          case Left(r) => Eff.Impure(r, Arrs.Leaf(x => handleRelayS[F, R, S, A, B](state, k(x))(handler)))
        }
    }

  def run[A](eff: Eff[Nothing, A]): A =
    (eff: @unchecked) match {
      case Pure(a) => a.asInstanceOf[A]
    }
}
