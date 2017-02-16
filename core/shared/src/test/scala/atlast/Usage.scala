package atlast

import cats.{Monad, ~>}
import cats.implicits._

object Usage {

  trait Storage[F[_]] {
    def get(key: String): F[Option[String]]

    def put(key: String, data: String): F[Unit]

    def remove(key: String): F[Unit]
  }

  object Storage {

    def get[Effs](key: String)(implicit ev: <=[Storage, Effs]): ATM[Effs, Option[String]] =
      new ATM[Effs, Option[String]] {
        override def run[F[_] : Monad](tf: InterpretersTo[F, Effs]): F[Option[String]] = ev.extract(tf).get(key)
      }

    def put[Effs](key: String, data: String)(implicit ev: <=[Storage, Effs]): ATM[Effs, Unit] =
      new ATM[Effs, Unit] {
        override def run[F[_] : Monad](tf: InterpretersTo[F, Effs]): F[Unit] = ev.extract(tf).put(key, data)
      }

    def remove[Effs](key: String)(implicit ev: <=[Storage, Effs]): ATM[Effs, Unit] =
      new ATM[Effs, Unit] {
        override def run[F[_] : Monad](tf: InterpretersTo[F, Effs]): F[Unit] = ev.extract(tf).remove(key)
      }

  }

  object LsStorage {
    def lsKeys[Effs](implicit ev: <=[LsStorage, Effs]): ATM[Effs, Iterator[String]] =
      new ATM[Effs, Iterator[String]] {
        override def run[F[_] : Monad](tf: InterpretersTo[F, Effs]): F[Iterator[String]] = ev.extract(tf).lsKeys
      }
  }


  trait LsStorage[F[_]] {
    def lsKeys: F[Iterator[String]]
  }

  type MyEffs = Fx.Two[Storage, Option ~> ?[_]]

  def getAndRemove[R
  : Storage <= ?
  : (Option ~> ?[_]) <= ?
  ](key: String): ATM[R, String] = for {
    outOption <- Storage.get(key)
    out <- ATM.to(outOption)
    _ <- Storage.remove(key)
  } yield out

}

