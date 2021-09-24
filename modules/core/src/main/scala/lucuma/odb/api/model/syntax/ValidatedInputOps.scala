// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package syntax

import cats.{Applicative, ApplicativeError}
import cats.syntax.functor._
import cats.syntax.validated._

final class ValidatedInputOps[A](self: ValidatedInput[A]) {

  def liftTo[F[_]](implicit F: ApplicativeError[F, Throwable]): F[A] =
    self.leftMap(nec => InputError.Exception(nec)).liftTo[F]

  def flatten[B](implicit ev: A <:< ValidatedInput[B]): ValidatedInput[B] =
    self.andThen(ev)

  def flatTraverse[F[_]: Applicative, B](f: A => F[ValidatedInput[B]]): F[ValidatedInput[B]] =
    self.traverse(f).map(v => new ValidatedInputOps(v).flatten)

}

trait ToValidatedInputOps {
  implicit def toValidatedInputOps[A](v: ValidatedInput[A]): ValidatedInputOps[A] =
    new ValidatedInputOps[A](v)
}

object validatedinput extends ToValidatedInputOps
