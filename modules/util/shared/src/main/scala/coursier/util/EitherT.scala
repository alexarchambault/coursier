package coursier.util

import coursier.util.Monad.ops._

final case class EitherT[F[+_], +L, +R](run: F[Either[L, R]]) {

  def map[S](f: R => S)(implicit M: Monad[F]): EitherT[F, L, S] =
    EitherT(
      run.map(_.map(f))
    )

  def flatMap[L0 >: L, S](f: R => EitherT[F, L0, S])(implicit M: Monad[F]): EitherT[F, L0, S] =
    EitherT[F, L0, S](
      run.flatMap {
        case Left(l) =>
          M.point(Left(l))
        case Right(r) =>
          f(r).run
      }
    )

  def leftMap[M](f: L => M)(implicit M: Monad[F]): EitherT[F, M, R] =
    EitherT(
      run.map(_.left.map(f))
    )

  def leftFlatMap[S, R0 >: R](f: L => EitherT[F, S, R0])(implicit M: Monad[F]): EitherT[F, S, R0] =
    EitherT[F, S, R0](
      run.flatMap {
        case Left(l) =>
          f(l).run
        case Right(r) =>
          M.point(Right(r))
      }
    )

  def orElse[L0 >: L, R0 >: R](other: => EitherT[F, L0, R0])(implicit
    M: Monad[F]
  ): EitherT[F, L0, R0] =
    EitherT[F, L0, R0](
      run.flatMap {
        case Left(_) =>
          other.run
        case Right(r) =>
          M.point(Right(r))
      }
    )

}

object EitherT {

  def point[F[+_], L, R](r: R)(implicit M: Monad[F]): EitherT[F, L, R] =
    EitherT[F, L, R](M.point(Right(r)))

  def fromEither[F[+_]]: FromEither[F] =
    new FromEither[F]

  final class FromEither[F[+_]] {
    def apply[L, R](either: Either[L, R])(implicit M: Monad[F]): EitherT[F, L, R] =
      EitherT(M.point(either))
  }

}
