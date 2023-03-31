package kits.eff

import izumi.reflect.TagK

object State {
  def run[R[_], S, A](state: S)(eff: Eff[[X] =>> State[S, X] | R[X], A]): Eff[R, (S, A)] =
    Eff.handleRelayS[[X] =>> State[S, X], R, S, A, (S, A)](state, eff)(new Eff.HandlerS[[X] =>> State[S, X], R, S, A, (S, A)] {
      def pure(s: S, a: A) = Eff.Pure((s, a))
      def flatMap[T](s: S, fa: State[S, T])(k: (S, T) => Eff[R, (S, A)]) =
        fa match {
          case Reader.Get() => k(s, s)
          case Writer.Put(s) => k(s, ())
        }
    })(TagK[[X] =>> State[S, X]])
}
