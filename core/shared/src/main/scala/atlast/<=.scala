package atlast

import atlast.Fx.{Append, One, Three, Two}

final case class <=[T[_[_]], Effs](index: Int) extends AnyVal {
  def extract[F[_]](effs: InterpretersTo[F, Effs]): T[F] =
    effs.coll(index).asInstanceOf[T[F]]
  def inject[F[_]](tf: T[F], effs: InterpretersTo[F, Effs]): InterpretersTo[F, Effs] =
    InterpretersTo[F, Effs](effs.coll.updated(index, tf))
}

object <= extends ExtractAppendRPriority {

  val extractZeroUnsafe: <=[Nothing, Fx] =
    <=[Nothing, Fx](0)

  val extractOneUnsafe: <=[Nothing, Fx] =
    <=[Nothing, Fx](1)

  val extractTwoUnsafe: <=[Nothing, Fx] =
    <=[Nothing, Fx](2)

}

private[atlast] trait ExtractZeroPriority {

  implicit def extractOne[T[_[_]]]: <=[T, One[T]] =
    <=.extractZeroUnsafe.asInstanceOf[<=[T, One[T]]]

  implicit def extractTwoR[T[_[_]], U[_[_]]]: <=[U, Two[T, U]] =
    <=.extractZeroUnsafe.asInstanceOf[<=[U, Two[T, U]]]

  implicit def extractThreeR[T[_[_]], U[_[_]], V[_[_]]]: <=[V, Three[T, U, V]] =
    <=.extractZeroUnsafe.asInstanceOf[<=[V, Three[T, U, V]]]

}

private[atlast] trait ExtractOnePriority extends ExtractZeroPriority {

  implicit def extractTwoL[T[_[_]], U[_[_]]]: <=[T, Two[T, U]] =
    <=.extractOneUnsafe.asInstanceOf[<=[T, Two[T, U]]]

  implicit def extractThreeM[T[_[_]], U[_[_]], V[_[_]]]: <=[U, Three[T, U, V]] =
    <=.extractOneUnsafe.asInstanceOf[<=[U, Three[T, U, V]]]

}

private[atlast] trait ExtractTwoPriority extends ExtractOnePriority {
  implicit def extractThreeL[T[_[_]], U[_[_]], V[_[_]]]: <=[T, Three[T, U, V]] =
    <=.extractTwoUnsafe.asInstanceOf[<=[T, Three[T, U, V]]]
}

private[atlast] trait ExtractAppendLPriority extends ExtractTwoPriority {
  implicit def extractAppendL[T[_[_]], L, R](implicit L: <=[T, L], R: Size[R]): <=[T, Append[L, R]] =
    <=(L.index + R.size)
}

private[atlast] trait ExtractAppendRPriority extends ExtractAppendLPriority {
  implicit def extractAppendR[T[_[_]], L, R](implicit R: <=[T, R]): <=[T, Append[L, R]] =
    R.asInstanceOf[<=[T, Append[L, R]]]
}
