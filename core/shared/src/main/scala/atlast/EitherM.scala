package atlast

trait EitherM[L, F[_]] {
  def left[R](l: L): F[R]
}
