package kits.eff

import izumi.reflect.TagK

enum Reader[I, A] {
  case Get[I]() extends Reader[I, I]
}

object Reader {
  def ask[I]: Eff[[A] =>> Reader[I, A], I] = {
    Eff[[A] =>> Reader[I, A], I](Get())(TagK[[X] =>> Reader[I, X]])
  }

  def local[I, R[a] >: Reader[I, a], A](f: I => I)(eff: Eff[R, A]): Eff[R, A] =
    ask.flatMap[R, A] { r0 =>
      val r = f(r0)
      run(r)(eff)
    }

  def run[R[_], I, A](value: I)(eff: Eff[[X] =>> Reader[I, X] | R[X], A]): Eff[R, A] = {
    Eff.handleRelay[[X] =>> Reader[I, X], R, A, A](eff)(new Eff.Handler[[X] =>> Reader[I, X], R, A, A] {
      def pure(a: A): Eff[R, A] = Eff.Pure(a)
      def flatMap[T](fa: Reader[I, T])(k: T => Eff[R, A]): Eff[R, A] =
        fa match {
          case Reader.Get() => k(value)
        }
    })(TagK[[X] =>> Reader[I, X]])
  }
}
