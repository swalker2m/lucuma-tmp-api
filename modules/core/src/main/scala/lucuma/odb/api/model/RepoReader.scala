// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.mtl.{Raise, Stateful}
import cats.syntax.all._
import cats.{Applicative, Functor, Monad}
import lucuma.core.util.Gid
import monocle.{Getter, Lens}

trait RepoReader[T, A, B] extends Serializable { self =>

  def isDefinedAt[F[_]](a: A)(implicit S: Stateful[F, T]): F[Boolean]

  def isEmptyAt[F[_]: Functor](id: A)(implicit S: Stateful[F, T]): F[Boolean] =
    isDefinedAt(id).map { r => !r }

  def lookupOrRaise[F[_]: Monad](a: A)(implicit G: Gid[A], R: Raise[F, InputError], S: Stateful[F, T]): F[B]

  def lookupValidated[F[_]: Functor](a: A)(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[B]]

  def lookupAllValidated[F[_]: Applicative](as: List[A])(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[List[B]]] =
    as.traverse(lookupValidated[F](_)).map(_.sequence)

  def lookupOption[F[_]: Functor](a: A)(implicit G: Gid[A], S: Stateful[F, T]): F[Option[B]]

  def findAll[F[_]: Functor](f: ((A, B)) => Boolean)(implicit S: Stateful[F, T]): F[List[B]]

}

object RepoReader {

  def fromGetter[T, A, B](g: Getter[T, Map[A, B]]): RepoReader[T, A, B] =
    new RepoReader[T, A, B] {
      override def isDefinedAt[F[_]](a: A)(implicit S: Stateful[F, T]): F[Boolean] =
        S.inspect(t => g.get(t).contains(a))

      def missingReference[F[_]](a: A)(implicit G: Gid[A]): InputError =
        InputError.fromMessage(s"missing reference `${G.show(a)}``")

      override def lookupOrRaise[F[_]: Monad](a: A)(implicit G: Gid[A], R: Raise[F, InputError], S: Stateful[F, T]): F[B] =
        for {
          t <- S.get
          b <- g.get(t).get(a).fold(R.raise[InputError, B](missingReference(a)))(Monad[F].pure[B])
        } yield b

      override def lookupValidated[F[_] : Functor](a: A)(implicit G: Gid[A], S: Stateful[F, T]): F[ValidatedInput[B]] =
        lookupOption(a).map(_.toValidNec(missingReference(a)))

      override def lookupOption[F[_]: Functor](a: A)(implicit G: Gid[A], S: Stateful[F, T]): F[Option[B]] =
        S.inspect(g.get(_).get(a))

      override def findAll[F[_]: Functor](f: ((A, B)) => Boolean)(implicit S: Stateful[F, T]): F[List[B]] =
        S.inspect(g.get(_).filter(f).values.toList)

    }

  def fromLens[T, A, B](l: Lens[T, Map[A, B]]): RepoReader[T, A, B] =
    fromGetter(l.asGetter)

}