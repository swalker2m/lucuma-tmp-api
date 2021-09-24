// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.BoundedEnumerable
import cats.mtl.{Raise, Stateful}
import cats.syntax.all._
import cats.{Applicative, Functor, Monad}
import lucuma.core.util.Gid
import monocle.Lens

import scala.collection.immutable.SortedMap

trait RepoState[T, A, B] extends RepoReader[T, A, B] {

  def cycleNextUnused[F[_]: Monad](implicit G: Gid[A], S: Stateful[F, T]): F[A]

  def getUnusedId[F[_]: Monad](suggestion: Option[A])(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[A]]

  def saveNew[F[_]: Applicative](a: A, b: B)(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[Unit]]

  def saveNewIfValid[F[_]: Applicative](b: ValidatedInput[B])(id: B => A)(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[Unit]]

  def update[F[_]: Applicative](a: A, b: B)(implicit G: Gid[A], S: Stateful[F, T]): F[Unit]

  def delete[F[_]: Applicative](a: A)(implicit G: Gid[A], S: Stateful[F, T]): F[Unit]

}

object RepoState {

  def fromLenses[T, A, B](
    idLens:  Lens[T, A],
    mapLens: Lens[T, SortedMap[A, B]]
  ): RepoState[T, A, B] =
    new RepoState[T, A, B] {
      val reader: RepoReader[T, A, B] =
        RepoReader.fromGetter(mapLens.asGetter.widen[Map[A, B]])

      override def isDefinedAt[F[_]](a: A)(implicit S: Stateful[F, T]): F[Boolean] =
        reader.isDefinedAt(a)

      override def lookupOrRaise[F[_] : Monad](a: A)(implicit G: Gid[A], R: Raise[F, InputError], S: Stateful[F, T]): F[B] =
        reader.lookupOrRaise(a)

      override def lookupValidated[F[_] : Functor](a: A)(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[B]] =
        reader.lookupValidated(a)

      override def lookupOption[F[_] : Functor](a: A)(implicit G: Gid[A], S: Stateful[F, T]): F[Option[B]] =
        reader.lookupOption(a)

      override def findAll[F[_]: Functor](f: ((A, B)) => Boolean)(implicit S: Stateful[F, T]): F[List[B]] =
        reader.findAll(f)

      override def cycleNextUnused[F[_]: Monad](implicit G: Gid[A], S: Stateful[F, T]): F[A] = {
        val unused: F[Boolean] =
          for {
            i <- S.inspect(idLens.get)
            b <- isEmptyAt(i)
          } yield b

        for {
          _ <- S.modify(idLens.modify(BoundedEnumerable[A].cycleNext)).untilM_(unused)
          i <- S.inspect(idLens.get)
        } yield i
      }

      def alreadyDefined(a: A)(implicit G: Gid[A]): InputError =
        InputError.fromMessage(s"`${G.show(a)}` is already defined")

      override def getUnusedId[F[_]: Monad](suggestion: Option[A])(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[A]] =
        suggestion.fold(cycleNextUnused.map(_.validNec[InputError])) { id =>
          isEmptyAt(id).map { isEmpty =>
            if (isEmpty) id.validNec[InputError] else alreadyDefined(id).invalidNec[A]
          }
        }

      override def saveNew[F[_]: Applicative](a: A, b: B)(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[Unit]] =
        isDefinedAt(a).ifA(
          Applicative[F].pure(alreadyDefined(a).invalidNec[Unit]),
          update(a, b).map(_.validNec[InputError])
        )

      override def saveNewIfValid[F[_]: Applicative](b: ValidatedInput[B])(id: B => A)(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[Unit]] =
        b.fold(
          nec => Applicative[F].pure(nec.invalid[Unit]),
          v   => saveNew(id(v), v)
        )

      override def update[F[_]: Applicative](a: A, b: B)(implicit G: Gid[A], S: Stateful[F, T]): F[Unit] =
        S.modify(mapLens.modify(_ + (a -> b)))

      override def delete[F[_]: Applicative](a: A)(implicit G: Gid[A], S: Stateful[F, T]): F[Unit] =
        S.modify(mapLens.modify(_ - a))

    }


}