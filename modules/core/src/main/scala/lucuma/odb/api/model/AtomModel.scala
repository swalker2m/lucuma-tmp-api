// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Atom, Step}
import lucuma.odb.api.model.StepConfig.CreateStepConfig

import cats.{Applicative, Eq, Eval, Monad, Traverse}
import cats.data.{Nested, NonEmptyList}
import cats.mtl.Stateful
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class AtomModel[A](
  id:    Atom.Id,
  steps: NonEmptyList[A]
)

object AtomModel {

  def one[A](id: Atom.Id, head: A): AtomModel[A] =
    AtomModel(id, NonEmptyList.one(head))

  def ofSteps[A](id: Atom.Id, head: A, tail: A*): AtomModel[A] =
    AtomModel(id, NonEmptyList.of(head, tail: _*))

  implicit def EqAtomModel[A: Eq]: Eq[AtomModel[A]] =
    Eq.by { a => (
      a.id,
      a.steps
    )}

  implicit val TraverseAtomModel: Traverse[AtomModel] =
    new Traverse[AtomModel] {
      override def traverse[G[_], A, B](fa: AtomModel[A])(f: A => G[B])(implicit ev: Applicative[G]): G[AtomModel[B]] =
        fa.steps.traverse(f).map { steps => AtomModel(fa.id, steps) }

      override def foldLeft[A, B](fa: AtomModel[A], b: B)(f: (B, A) => B): B =
        fa.steps.foldLeft(b)(f)

      override def foldRight[A, B](fa: AtomModel[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.steps.foldRight(lb)(f)
    }

  def dereference[F[_]: Monad, T, D](db: DatabaseReader[T], id: Atom.Id)(f: StepConfig[_] => Option[D])(implicit S: Stateful[F, T]): F[Option[AtomModel[StepModel[D]]]] =
      db.atom
        .lookupOption[F](id)
        .flatMap(_.flatTraverse(_.dereference[F, T, D](db)(f)))

  implicit class ReferenceExtensions(a: AtomModel[Step.Id]) {

    def dereference[F[_]: Applicative, T, D](db: DatabaseReader[T])(f: StepConfig[_] => Option[D])(implicit S: Stateful[F, T]): F[Option[AtomModel[StepModel[D]]]] =
      a.steps
       .traverse(i => StepModel.dereference[F, T, D](db, i)(f))
       .map(_.sequence.map(AtomModel(a.id, _)))

  }


  final case class Create[A](
    id:    Option[Atom.Id],
    steps: List[StepModel.Create[A]]
  ) {

    def create[F[_]: Monad, T, B](db: DatabaseState[T])(implicit V: InputValidator[A, B], S: Stateful[F, T]): F[ValidatedInput[AtomModel[StepModel[B]]]] =
      steps match {
        case Nil    =>
          Monad[F].pure[ValidatedInput[AtomModel[StepModel[B]]]](
            InputError.fromMessage("Cannot create an emptySequence atom").invalidNec
          )

        case h :: t =>
          for {
            i  <- db.atom.getUnusedId(id)
            hʹ <- h.create[F, T, B](db)
            tʹ <- t.traverse(_.create[F, T, B](db))
            a   = (i, hʹ, tʹ.sequence).mapN { (iʹʹ, hʹʹ, tʹʹ) =>
              AtomModel.ofSteps(iʹʹ, hʹʹ, tʹʹ: _*)
            }
            _  <- db.atom.saveNewIfValid(Nested(a).map(_.id).value)(_.id)
          } yield a

      }
  }

  object Create {

    def singleton[A](step: StepModel.Create[A]): Create[A] =
      Create(None, List(step))

    def stopBefore[A](step: CreateStepConfig[A]): Create[A] =
      singleton(StepModel.Create.stopBefore(step))

    def continueTo[A](step: CreateStepConfig[A]): Create[A] =
      singleton(StepModel.Create.continueTo(step))

    implicit def DecoderCreate[A: Decoder]: Decoder[Create[A]] =
      deriveDecoder[Create[A]]

    implicit def EqCreate[A: Eq]: Eq[Create[A]] =
      Eq.by { a => (
        a.id,
        a.steps
      )}

  }

}
