import atlast.{ATM, Fx, InterpretersTo}
import cats.arrow.FunctionK
import cats.~>
import cats.implicits._
import org.scalatest.{FreeSpec, Matchers}

class ATMTest extends FreeSpec with Matchers {

  "unions" - {
    val noneMaker: Option ~> Option = new (Option ~> Option) {
      override def apply[A](fa: Option[A]): Option[A] = fa.flatMap(_ => None)
    }
    def onlyRights[L]: (Either[L, ?] ~> Option) = new (Either[L, ?] ~> Option) {
      override def apply[A](fa: L Either A): Option[A] = fa.right.toOption
    }
    def nonesThis[L](l: L): (Option ~> Either[L, ?]) = new (Option ~> Either[L, ?]) {
      override def apply[A](fa: Option[A]): L Either A = fa.fold(Either.left[L, A](l))(Either.right(_))
    }
    "single member" in {
      ATM.to[Fx.One[Option ~> ?[_]]](Option(1)).run(
        InterpretersTo.one(noneMaker)
      ) should be (empty)
    }
    "two of the same member" in {
      ATM.to[Fx.Two[Option ~> ?[_], Option ~> ?[_]]](Option(1)).run(
        InterpretersTo.two(FunctionK.id[Option], noneMaker)
      ) should be (Some(1))
    }
    "two different members" in {
      ATM.to[Fx.Two[Either[String, ?] ~> ?[_], Option ~> ?[_]]](Either.right[String, Int](1)).run(
        InterpretersTo.two(onlyRights, noneMaker)
      ) should be (Some(1))
      ATM.to[Fx.Two[Either[String, ?] ~> ?[_], Option ~> ?[_]]](None).run(
        InterpretersTo.two(FunctionK.id, nonesThis("hey"))
      ) should be (Left("hey"))
    }
  }

}
