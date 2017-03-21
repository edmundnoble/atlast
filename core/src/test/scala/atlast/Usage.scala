package atlast

import cats.data.{OptionT, State}
import cats.implicits._
import cats.{Monad, Traverse, ~>}

object Usage {

  trait Storage[F[_]] {
    def get(key: String): F[Option[String]]

    def put(key: String, data: String): F[Unit]

    def remove(key: String): F[Unit]
  }

  trait LsStorage[F[_]] {
    def lsKeys: F[Iterator[String]]
  }

  trait RMonad[F[_], I[_]] {
    def inject[A](ia: I[A]): F[A]

    def projMap[A, B](fa: F[A])(f: I[A] => F[B]): F[B]

    final def dematerialize[A](fia: F[I[A]])(implicit F: Monad[F]): F[A] = F.flatMap(fia)(inject)

    final def materialize[A](fa: F[A])(implicit F: Monad[F]): F[I[A]] = projMap(fa)(F.pure)
  }

  type of[T[_], F[_]] = {type l[A] = T[F[A]]}

  object RMonad {
    implicit def selfRMonad[F[_]]: RMonad[F, F] = new RMonad[F, F] {
      override def inject[A](ia: F[A]): F[A] = ia

      override def projMap[A, B](fa: F[A])(f: (F[A]) => F[B]): F[B] = f(fa)
    }

    type Curried[F[_]] = {type l[A] = OptionT[F, A]}

    implicit def optionTRMonad[F[_]](implicit F: Monad[F]): RMonad[Curried[F]#l, Option] = new RMonad[Curried[F]#l, Option] {
      override def inject[A](ia: Option[A]): OptionT[F, A] = OptionT.fromOption[F](ia)
      override def projMap[A, B](fa: OptionT[F, A])(f: (Option[A]) => OptionT[F, B]): OptionT[F, B] = OptionT(F.flatMap(fa.value)(f(_).value))
    }

    implicit def traverseRMonad[F[_], T[_]](implicit F: Monad[F]): RMonad[(F of T)#l, T] = new RMonad[(F of T)#l, T] {
      override def inject[A](ia: T[A]): F[T[A]] = F.pure(ia)
      override def projMap[A, B](fa: F[T[A]])(f: (T[A]) => F[T[B]]): F[T[B]] = fa.flatMap(f(_))
    }
  }

  def getAndRemove[F[_]](key: String)(implicit F: Monad[F],
                                      storage: Storage[F],
                                      optionM: RMonad[F, Option]): F[String] = for {
    out <- optionM.dematerialize(storage.get(key))
    _ <- storage.remove(key)
  } yield out

  type Curried[S] = {type l[A] = State[S, A]}

  def stateStorage[F[_]](rm: Curried[Map[String, String]]#l ~> F) = new Storage[F] with LsStorage[F] {
    override def get(key: String): F[Option[String]] = rm(State.get[Map[String, String]].map(_.get(key)))
    override def put(key: String, data: String): F[Unit] = rm(State.modify[Map[String, String]](_ + (key -> data)))
    override def remove(key: String): F[Unit] = rm(State.modify[Map[String, String]](_ - key))
    override def lsKeys: F[Iterator[String]] = rm(State.get[Map[String, String]].map(_.keysIterator))
  }

}

