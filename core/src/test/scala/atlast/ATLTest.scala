import atlast.{EitherM, OptionM}
import cats.arrow.FunctionK
import cats.implicits._
import cats.{Monad, ~>}
import org.scalatest.{FreeSpec, Matchers}

class ATMTest extends FreeSpec with Matchers {

  "unions" - {
    "single member" in {
      def idder[F[_], A](fa: F[A])(implicit F: Monad[F], O: OptionM[F]): F[A] = fa
      def noner[F[_], A](fa: F[A])(implicit F: Monad[F], O: OptionM[F]): F[A] = O.none
      idder(Some(1)) shouldBe Some(1)
      noner(Some(1)) shouldBe empty
    }
    "two different members" in {
      implicit def nonesThis[F[_], L](l: L)(implicit E: EitherM[L, F]) = new OptionM[F] {
        override def none[A]: F[A] = E.left(l)
        override def some[A](r: A): F[A] = E.right(r)
      }
      implicit def onlyRights[F[_], L, A](implicit O: OptionM[F]) = new EitherM[L, F] {
        override def left[R](l: L): F[R] = O.none
        override def right[R](r: R): F[R] = O.some(r)
      }
    }
  }

}
