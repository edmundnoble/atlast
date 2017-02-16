package atlast

import cats.{Monad, ~>}

  final case class InterpretersTo[F[_], S] (coll: Vector[T[F] forSome {type T[_[_]]}])
    extends AnyVal

  object InterpretersTo {
    def one[T[_[_]], F[_]](tf: T[F]): InterpretersTo[F, Fx.One[T]] =
      InterpretersTo[F, Fx.One[T]](Vector.empty :+ tf)
    def two[T[_[_]], U[_[_]], F[_]](tf: T[F], uf: U[F]): InterpretersTo[F, Fx.Two[T, U]] =
      InterpretersTo[F, Fx.Two[T, U]](Vector[X[F] forSome {type X[_[_]]}](uf, tf))
    def three[T[_[_]], U[_[_]], V[_[_]], F[_]](tf: T[F], uf: U[F], vf: V[F]): InterpretersTo[F, Fx.Three[T, U, V]] =
      InterpretersTo[F, Fx.Three[T, U, V]](Vector[X[F] forSome {type X[_[_]]}](vf, uf, tf))
    def append[L, R, F[_]](le: InterpretersTo[F, L], re: InterpretersTo[F, R]): InterpretersTo[F, Fx.Append[L, R]] =
      InterpretersTo[F, Fx.Append[L, R]](le.coll ++ re.coll)
  }

  trait ATM[Fx, A] {
    def run[F[_] : Monad](tf: InterpretersTo[F, Fx]): F[A]
  }

  object ATM {
    type Curried[Fx] = {type l[A] = ATM[Fx, A]}

    final class toPartiallyApplied[Fx](val ignored: Int) extends AnyVal {
      def apply[T[_], A](ta: T[A])(implicit extract: (T ~> ?[_]) <= Fx): ATM[Fx, A] =
        new ATM[Fx, A] {
          override def run[F[_] : Monad](tf: InterpretersTo[F, Fx]): F[A] =
            extract.extract[F](tf).apply(ta)
        }
    }

    def to[Fx] = new toPartiallyApplied[Fx](1)

    implicit def effStackMonad[Fx]: Monad[Curried[Fx]#l] =
      effStackMonadUnsafe.asInstanceOf[Monad[Curried[Fx]#l]]

    val effStackMonadUnsafe: Monad[Curried[Fx]#l] = new Monad[Curried[Fx]#l] {
      override def pure[A](a: A): ATM[Fx, A] = new ATM[Fx, A] {
        override def run[F[_]](tf: InterpretersTo[F, Fx])(implicit F: Monad[F]): F[A] = F.pure(a)
      }
      override def flatMap[A, B](fa: ATM[Fx, A])(f: (A) => ATM[Fx, B]): ATM[Fx, B] = new ATM[Fx, B] {
        override def run[F[_]](tf: InterpretersTo[F, Fx])(implicit F: Monad[F]): F[B] = F.flatMap(fa.run(tf))(f(_).run(tf))
      }
      override def tailRecM[A, B](a: A)(f: (A) => ATM[Fx, Either[A, B]]): ATM[Fx, B] = new ATM[Fx, B] {
        override def run[F[_]](tf: InterpretersTo[F, Fx])(implicit F: Monad[F]): F[B] = F.tailRecM(a)(f(_).run(tf))
      }
    }
  }

