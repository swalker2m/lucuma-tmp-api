// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Applicative
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string._

object ValidatedInput {

  def requireOne[A](name: String, a: Option[ValidatedInput[A]]*): ValidatedInput[A] =
    requireOne(name, a.toList)

  def requireOne[A](name: String, as: List[Option[ValidatedInput[A]]]): ValidatedInput[A] =
    as.flattenOption match {
      case List(a) => a
      case Nil     => InputError.missingInput(name).invalidNec[A]
      case _       => InputError.fromMessage(s"Multiple '$name' definitions are not permitted").invalidNec[A]
    }

  def requireOneF[F[_]: Applicative, A](
    name: String,
    a:    Option[F[ValidatedInput[A]]]*
  ): F[ValidatedInput[A]] =
    requireOneF[F, A](name, a.toList)

  def requireOneF[F[_]: Applicative, A](
    name: String,
    as:   List[Option[F[ValidatedInput[A]]]]
  ): F[ValidatedInput[A]] =
    as.flattenOption match {
      case List(a) => a
      case Nil     => Applicative[F].pure(InputError.missingInput(name).invalidNec[A])
      case _       => Applicative[F].pure(InputError.fromMessage(s"Multiple '$name' definitions are not permitted").invalidNec[A])
    }

  def optionEither[A, B](
    nameA: String,
    nameB: String,
    a:     ValidatedInput[Option[A]],
    b:     ValidatedInput[Option[B]]
  ): ValidatedInput[Option[Either[A, B]]] =
    (a, b).mapN {
      case (Some(aʹ), None    ) => aʹ.asLeft[B].some.validNec[InputError]
      case (None,     Some(bʹ)) => bʹ.asRight[A].some.validNec[InputError]
      case (Some(_),  Some(_) ) => InputError.fromMessage(s"Either $nameA or $nameB are permitted but not both").invalidNec
      case (None,     None    ) => Option.empty[Either[A, B]].validNec[InputError]
    }.valueOr(_.invalid)

  def nonEmptyString(name: String, s: String): ValidatedInput[NonEmptyString] =
    NonEmptyString
      .from(s)
      .leftMap(err => InputError.fromMessage(s"'$name' may not be empty: $err"))
      .toValidatedNec

  def closedInterval[T](
    name:  String,
    value: T,
    low:   T,
    high:  T
  )(
    implicit v: Validate[T, Interval.Closed[low.type, high.type]]
  ): ValidatedInput[T Refined Interval.Closed[low.type, high.type]] =

    refineV[Interval.Closed[low.type, high.type]](value)
      .leftMap(_ => InputError.fromMessage(s"'$name' out of range: must be $low <= $name <= $high"))
      .toValidatedNec
}
