package atlast

import cats.{Applicative, ~>}
import cats.implicits._

trait OptionM[F[_]] {
  def none[A]: F[A]
}

object OptionM {
  def optionConsumer[F[_]: Applicative](opt: OptionM[F]): Option ~> F = new (Option ~> F) {
    override def apply[A](fa: Option[A]): F[A] = fa.fold(opt.none[A])(_.pure)
  }
}
