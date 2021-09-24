// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.{Eq, Monad}
import cats.mtl.Stateful
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.odb.api.model.syntax.input._
import lucuma.core.model.Program
import lucuma.core.optics.syntax.lens._
import monocle.Lens


/**
 * A placeholder Program for now.
 */
final case class ProgramModel(
  id:            Program.Id,
  existence:     Existence,
  name:          Option[NonEmptyString]
)

object ProgramModel extends ProgramOptics {

  implicit val TopLevelProgram: TopLevelModel[Program.Id, ProgramModel] =
    TopLevelModel.instance(_.id, ProgramModel.existence)

  implicit val EqProgram: Eq[ProgramModel] =
    Eq.by(p => (p.id, p.existence, p.name))

  /**
   * Program creation input class.
   */
  final case class Create(
    programId:     Option[Program.Id],
    name:          Option[NonEmptyString]
  ) {

    def create[F[_]: Monad, T](db: DatabaseState[T])(implicit S: Stateful[F, T]): F[ValidatedInput[ProgramModel]] = {

      for {
        i <- db.program.getUnusedId[F](programId)
        p  = i.map { pid => ProgramModel(pid, Existence.Present, name) }
        _ <- db.program.saveNewIfValid[F](p)(_.id)
      } yield p
    }

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

  }

  final case class Edit(
    programId:     Program.Id,
    existence:     Input[Existence]                   = Input.ignore,
    name:          Input[NonEmptyString]              = Input.ignore
  ) {

    def edit(p: ProgramModel): ValidatedInput[ProgramModel] = {
      existence.validateIsNotNull("existence").map { e =>
        (ProgramModel.existence := e).void
      }.map(_.runS(p).value)
    }

  }

  object Edit {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] =
      deriveConfiguredDecoder[Edit]

    implicit val EqEdit: Eq[Edit] =
      Eq.by { a => (
        a.programId,
        a.existence,
        a.name
      )}

  }

  final case class ProgramEvent (
    id:       Long,
    editType: Event.EditType,
    value:    ProgramModel,
  ) extends Event.Edit[ProgramModel]

  object ProgramEvent {
    def created(value: ProgramModel)(id: Long): ProgramEvent =
      ProgramEvent(id, Event.EditType.Created, value)

    def updated(value: ProgramModel)(id: Long): ProgramEvent =
      ProgramEvent(id, Event.EditType.Updated, value)
  }


}

trait ProgramOptics { self: ProgramModel.type =>

  val id: Lens[ProgramModel, Program.Id] =
    Lens[ProgramModel, Program.Id](_.id)(a => _.copy(id = a))

  val existence: Lens[ProgramModel, Existence] =
    Lens[ProgramModel, Existence](_.existence)(a => _.copy(existence = a))

  val name: Lens[ProgramModel, Option[NonEmptyString]] =
    Lens[ProgramModel, Option[NonEmptyString]](_.name)(a => _.copy(name = a))

}