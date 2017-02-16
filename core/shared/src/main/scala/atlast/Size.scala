package atlast

import scala.annotation.inductive

@inductive
final case class Size[Effs](size: Int) extends AnyVal

object Size {

  import Fx._

  private val sizeZeroUnsafe: Size[Fx] = Size(0)
  private val sizeOneUnsafe: Size[Fx] = Size(1)
  private val sizeTwoUnsafe: Size[Fx] = Size(2)
  private val sizeThreeUnsafe: Size[Fx] = Size(3)
  implicit def sizeZero[T[_[_]]]: Size[Zero] =
    sizeZeroUnsafe.asInstanceOf[Size[Zero]]
  implicit def sizeOne[T[_[_]]]: Size[One[T]] =
    sizeOneUnsafe.asInstanceOf[Size[One[T]]]
  implicit def sizeTwo[T[_[_]], U[_[_]]]: Size[Two[T, U]] =
    sizeTwoUnsafe.asInstanceOf[Size[Two[T, U]]]
  implicit def sizeThree[T[_[_]], U[_[_]], V[_[_]]]: Size[Three[T, U, V]] =
    sizeThreeUnsafe.asInstanceOf[Size[Three[T, U, V]]]
  implicit def sizeAppendL[L, R](implicit R: Size[R], L: Size[L]): Size[Append[L, R]] =
    Size(R.size + L.size)
}