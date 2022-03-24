package kits.eff

object Main {
  val hoge: Eff[[A] =>> Reader[Int, A] | Writer[String, A], Int] = for {
    i <- Reader.ask[Int]
    _ <- Writer.tell(i.toString)
  } yield i + 1

  val fuga: Eff[[A] =>> State[Int, A], Int] = for {
    i <- Reader.ask[Int]
    _ <- Writer.tell(i + 1)
    j <- Reader.ask[Int]
  } yield j + 1

  def main(args: Array[String]): Unit = {
    val r1 = Eff.run(Writer.runList(Reader.run(0)(hoge)))
    val r2 = Eff.run(Reader.run(0)(Writer.runList(hoge)))
    assert(r1 == (List("0") -> 1))
    assert(r1 == r2)

    val r3 = Eff.run(State.run(0)(fuga))
    assert(r3 == (1 -> 2))

//    val b = new Bench

    val arrs = Bench.benchEff(1 to 10000).asInstanceOf[Eff.Impure[[A] =>> State[Int, A], Int, Int]].arrs
    //pprint.pprintln(arrs)
    val result = arrs(1)
    //pprint.pprintln(result)
    /*
    val result = loop(arrs, f => TailRec.Return(f(1)))
    pprint.pprintln(result)
*/
    /*
        import scala.aannotationnnotation.tailrec

        sealed trait MyList[+A]
        case object MyNil extends MyList[Nothing]
        case class MyCons[+A](head : A, tail : MyList[A]) extends MyList[A]

        def apply[A] (as : A*) : MyList[A] = {
          @tailrec
          def loop(k: MyList[A] => TailRec[MyList[A]], xs : Seq[A]): TailRec[MyList[A]] =
            if (xs.isEmpty) k(MyNil)
            else loop( x => {
              val d = TailRec.Suspend(
                () => k(MyCons(xs.head, x))
              )
              d
            }, xs.tail)

          loop( x => TailRec.Return(x), as ).run
        }


        def apply2[A](as : A*) : MyList[A] =
          if (as.isEmpty) MyNil
          else MyCons(as.head, apply2(as.tail : _*))

        try {
          apply((1 to 10000).toSeq : _* )
        } catch {
          case e =>
            println(e)
        }
    */
  }
}
