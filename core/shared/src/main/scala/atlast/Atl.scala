package atlast

import cats.implicits._
import cats.{Monad, ~>}

import scala.annotation.inductive

object Test {

  final case class AppliedEffStack[F[_]](coll: Vector[T[F] forSome {type T[_[_]]}])
    extends AnyVal

  trait EffStack

  final case class Zero() extends EffStack

  final case class One[T[_[_]]]() extends EffStack

  final case class Two[T[_[_]], U[_[_]]]() extends EffStack

  final case class Three[T[_[_]], U[_[_]], V[_[_]]]() extends EffStack

  final case class AppendL[L, R]() extends EffStack

  final case class AppendR[L, R]() extends EffStack

  @inductive
  final case class Size[Effs](size: Int) extends AnyVal

  object Size {
    private val sizeZeroUnsafe: Size[EffStack] = Size(0)
    private val sizeOneUnsafe: Size[EffStack] = Size(1)
    private val sizeTwoUnsafe: Size[EffStack] = Size(2)
    private val sizeThreeUnsafe: Size[EffStack] = Size(3)
    implicit def sizeZero[T[_[_]]]: Size[Zero] =
      sizeZeroUnsafe.asInstanceOf[Size[Zero]]
    implicit def sizeOne[T[_[_]]]: Size[One[T]] =
      sizeOneUnsafe.asInstanceOf[Size[One[T]]]
    implicit def sizeTwo[T[_[_]], U[_[_]]]: Size[Two[T, U]] =
      sizeTwoUnsafe.asInstanceOf[Size[Two[T, U]]]
    implicit def sizeThree[T[_[_]], U[_[_]], V[_[_]]]: Size[Three[T, U, V]] =
      sizeThreeUnsafe.asInstanceOf[Size[Three[T, U, V]]]
    implicit def sizeAppendL[L, R](implicit R: Size[R], L: Size[L]): Size[AppendL[L, R]] =
      Size(R.size + L.size)
    implicit def sizeAppendR[L, R](implicit R: Size[R]): Size[AppendR[L, R]] =
      R.asInstanceOf[Size[AppendR[L, R]]]
  }

  @inductive
  final case class <=[T[_[_]], Effs](index: Int) extends AnyVal {
    def extract[F[_]](effs: AppliedEffStack[F]): T[F] =
      effs.coll(index).asInstanceOf[T[F]]
  }

  object <= {
    private val extractZeroUnsafe: <=[Nothing, EffStack] =
      <=[Nothing, EffStack](0)
    private val extractOneUnsafe: <=[Nothing, EffStack] =
      <=[Nothing, EffStack](1)
    private val extractTwoUnsafe: <=[Nothing, EffStack] =
      <=[Nothing, EffStack](2)
    implicit def extractOne[T[_[_]]]: <=[T, One[T]] =
      extractZeroUnsafe.asInstanceOf[<=[T, One[T]]]
    implicit def extractTwoL[T[_[_]], U[_[_]]]: <=[T, Two[T, U]] =
      extractOneUnsafe.asInstanceOf[<=[T, Two[T, U]]]
    implicit def extractTwoR[T[_[_]], U[_[_]]]: <=[U, Two[T, U]] =
      extractZeroUnsafe.asInstanceOf[<=[U, Two[T, U]]]
    implicit def extractThreeL[T[_[_]], U[_[_]], V[_[_]]]: <=[T, Three[T, U, V]] =
      extractTwoUnsafe.asInstanceOf[<=[T, Three[T, U, V]]]
    implicit def extractThreeM[T[_[_]], U[_[_]], V[_[_]]]: <=[U, Three[T, U, V]] =
      extractOneUnsafe.asInstanceOf[<=[U, Three[T, U, V]]]
    implicit def extractThreeR[T[_[_]], U[_[_]], V[_[_]]]: <=[V, Three[T, U, V]] =
      extractZeroUnsafe.asInstanceOf[<=[V, Three[T, U, V]]]
    implicit def extractAppendL[T[_[_]], L, R](implicit L: <=[T, L], R: Size[R]): <=[T, AppendL[L, R]] =
      <=(L.index + R.size)
    implicit def extractAppendR[T[_[_]], L, R](implicit R: <=[T, L]): <=[T, AppendR[L, R]] =
      R.asInstanceOf[<=[T, AppendR[L, R]]]
  }

  type MyEffs = Two[Storage, LsStorage]

  trait Storage[F[_]] {
    def get(key: String): F[Option[String]]

    def put(key: String, data: String): F[Unit]

    def remove(key: String): F[Unit]
  }

  object Storage {
    def get[Effs](key: String)(implicit ev: <=[Storage, Effs]): ATM[Effs, Option[String]] =
      new ATM[Effs, Option[String]] {
        override def apply[F[_] : Monad](tf: AppliedEffStack[F]): F[Option[String]] = ev.extract(tf).get(key)
      }
    def put[Effs](key: String, data: String)(implicit ev: <=[Storage, Effs]): ATM[Effs, Unit] =
      new ATM[Effs, Unit] {
        override def apply[F[_] : Monad](tf: AppliedEffStack[F]): F[Unit] = ev.extract(tf).put(key, data)
      }
    def remove[Effs](key: String)(implicit ev: <=[Storage, Effs]): ATM[Effs, Unit] =
      new ATM[Effs, Unit] {
        override def apply[F[_] : Monad](tf: AppliedEffStack[F]): F[Unit] = ev.extract(tf).remove(key)
      }
  }

  object LsStorage {
    def lsKeys[Effs](implicit ev: <=[LsStorage, Effs]): ATM[Effs, Iterator[String]] =
      new ATM[Effs, Iterator[String]] {
        override def apply[F[_] : Monad](tf: AppliedEffStack[F]): F[Iterator[String]] = ev.extract(tf).lsKeys
      }
  }


  trait LsStorage[F[_]] {
    def lsKeys: F[Iterator[String]]
  }

  trait ATL[C[_[_]], Effs, A] {
    def apply[F[_] : C](tf: AppliedEffStack[F]): F[A]
  }

  type ATM[Effs, A] = ATL[Monad, Effs, A]

  object ATL {
    type Curried[Effs] = {type l[A] = ATM[Effs, A]}
    def from[T[_], Effs: <=[T ~> ?[_], ?], A](ta: T[A]): ATM[Effs, A] =
      new ATM[Effs, A] {
        override def apply[F[_] : Monad](tf: AppliedEffStack[F]): F[A] =
          implicitly[<=[T ~> ?[_], Effs]].extract(tf)(ta)
      }

    implicit def effStackMonad[Effs]: Monad[Curried[Effs]#l] =
      effStackMonadUnsafe.asInstanceOf[Monad[Curried[Effs]#l]]

    val effStackMonadUnsafe: Monad[Curried[EffStack]#l] = new Monad[Curried[EffStack]#l] {
      override def pure[A](a: A): ATM[EffStack, A] = new ATM[EffStack, A] {
        override def apply[F[_]](tf: AppliedEffStack[F])(implicit F: Monad[F]): F[A] = F.pure(a)
      }
      override def flatMap[A, B](fa: ATM[EffStack, A])(f: (A) => ATM[EffStack, B]): ATM[EffStack, B] = new ATM[EffStack, B] {
        override def apply[F[_]](tf: AppliedEffStack[F])(implicit F: Monad[F]): F[B] = F.flatMap(fa(tf))(f(_).apply(tf))
      }
      override def tailRecM[A, B](a: A)(f: (A) => ATM[EffStack, Either[A, B]]): ATM[EffStack, B] = new ATM[EffStack, B] {
        override def apply[F[_]](tf: AppliedEffStack[F])(implicit F: Monad[F]): F[B] = F.tailRecM(a)(f(_)(tf))
      }
    }
  }

  object Usage {

    def getAndRemove[R
    : Storage <= ?
    : LsStorage <= ?
    : (Either[String, ?] ~> ?[_]) <= ?
    ](key: String): ATM[R, Option[String]] = for {
      out <- Storage.get(key)
      _ <- Storage.remove(key)
      _ <- ATL.from(Either.left[String, Any]("hello"))
    } yield out
  }

}
