package atlast

import cats.Monad

//import scala.annotation.inductive

object Test {

  trait AppliedEffStack[F[_]]

  final case class N[F[_]]() extends AppliedEffStack[F]
  final case class Cons[T[_[_]], F[_], Rest <: AppliedEffStack[F]](head: T[F], rest: Rest) extends AppliedEffStack[F]

  type MyEffs = Storage ->: LsStorage ->: OO

  trait EffStack {
    type Apply[F[_]] <: AppliedEffStack[F]
  }

  final case class OO() extends EffStack {
    override type Apply[F[_]] = N[F]
  }
  final case class ->:[T[_[_]], Rest <: EffStack]() extends EffStack {
    override type Apply[F[_]] = Cons[T, F, Rest#Apply[F]]
  }

  trait Storage[F[_]] {
    def get(key: String): F[Option[String]]

    def put(key: String, data: String): F[Unit]

    def remove(key: String): F[Unit]
  }

//  @inductive
  trait Extract[T[_[_]], Effs <: EffStack] {
    def extract[F[_]](effs: Effs#Apply[F]): T[F]
  }

  object Extract extends LowPrioExtract {
    implicit final def extractCons[T[_[_]], Effs <: EffStack]: Extract[T, T ->: Effs] = new Extract[T, T ->: Effs] {
      override def extract[F[_]](effs: Cons[T, F, Effs#Apply[F]]): T[F] = effs.head
    }
  }

  abstract class LowPrioExtract {
    implicit final def extractConsN[X[_[_]], T[_[_]], Effs <: EffStack](implicit ev: Extract[T, Effs]): Extract[T, X ->: Effs] = new Extract[T, X ->: Effs] {
      override def extract[F[_]](effs: Cons[X, F, Effs#Apply[F]]): T[F] = ev.extract(effs.rest)
    }
  }

  object Storage {
    def get[Effs <: EffStack](key: String)(implicit ev: Extract[Storage, Effs]): ATM[Effs, Option[String]] =
      new ATM[Effs, Option[String]] {
        override def apply[F[_]: Monad](tf: Effs#Apply[F]): F[Option[String]] = ev.extract(tf).get(key)
      }
    def put[Effs <: EffStack](key: String, data: String)(implicit ev: Extract[Storage, Effs]): ATM[Effs, Unit] =
      new ATM[Effs, Unit] {
        override def apply[F[_]: Monad](tf: Effs#Apply[F]): F[Unit] = ev.extract(tf).put(key, data)
      }
    def remove[Effs <: EffStack](key: String)(implicit ev: Extract[Storage, Effs]): ATM[Effs, Unit] =
      new ATM[Effs, Unit] {
        override def apply[F[_]: Monad](tf: Effs#Apply[F]): F[Unit] = ev.extract(tf).remove(key)
      }
  }

  trait LsStorage[F[_]] {
    def lsKeys: F[Iterator[String]]
  }

  trait ATL[C[_[_]], Effs <: EffStack, A] {
    def apply[F[_]: C](tf: Effs#Apply[F]): F[A]
  }

  type ATM[Effs <: EffStack, A] = ATL[Monad, Effs, A]

  object ATL {
    type Curried[Effs <: EffStack] = {type l[A] = ATM[Effs, A]}
    implicit def effStackMonad[Effs <: EffStack]: Monad[Curried[Effs]#l] =
      effStackMonadUnsafe.asInstanceOf[Monad[Curried[Effs]#l]]

    val effStackMonadUnsafe: Monad[Curried[EffStack]#l] = new Monad[Curried[EffStack]#l] {
      override def pure[A](a: A): ATM[EffStack, A] = new ATM[EffStack, A] {
        override def apply[F[_]](tf: EffStack#Apply[F])(implicit F: Monad[F]): F[A] = F.pure(a)
      }
      override def flatMap[A, B](fa: ATM[EffStack, A])(f: (A) => ATM[EffStack, B]): ATM[EffStack, B] = new ATM[EffStack, B] {
        override def apply[F[_]](tf: EffStack#Apply[F])(implicit F: Monad[F]): F[B] = F.flatMap(fa(tf))(f(_).apply(tf))
      }
      override def tailRecM[A, B](a: A)(f: (A) => ATM[EffStack, Either[A, B]]): ATM[EffStack, B] = new ATM[EffStack, B] {
        override def apply[F[_]](tf: EffStack#Apply[F])(implicit F: Monad[F]): F[B] = F.tailRecM(a)(f(_)(tf))
      }
    }
  }

}
