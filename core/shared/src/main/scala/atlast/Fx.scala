package atlast

trait Fx

object Fx {

  final case class Zero() extends Fx

  final case class One[T[_[_]]]() extends Fx

  final case class Two[T[_[_]], U[_[_]]]() extends Fx

  final case class Three[T[_[_]], U[_[_]], V[_[_]]]() extends Fx

  final case class Append[L, R]() extends Fx

}

