// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Order}
import cats.data.{StateT, Validated}
import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.traverse._
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.model.{Observation, Program, SourceProfile, Target}
import lucuma.odb.api.model.{Database, EditorInput, EitherInput, Event, Existence, InputError, TopLevelModel, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.targetModel.SourceProfileModel.SourceProfileInput
import monocle.{Focus, Lens}


/**
 * TargetModel pairs an id with a `lucuma.core.model.Target`.
 */
final case class TargetModel(
  id:        Target.Id,
  existence: Existence,
  programId: Program.Id,
  target:    Target,
  observed:  Boolean
) {

  def name: NonEmptyString =
    target.name

  def clone(newId: Target.Id): TargetModel =
    copy(id = newId, existence = Existence.Present, observed = false)

}

object TargetModel extends TargetModelOptics {

  implicit val TopLevelTargetModel: TopLevelModel[Target.Id, TargetModel] =
    TopLevelModel.instance(_.id, TargetModel.existence)

  implicit val OrderTargetModel: Order[TargetModel] = {
    implicit val nameOrder: Order[Target] = Target.NameOrder

    Order.by { a =>
      (
        a.id,
        a.existence,
        a.programId,
        a.target,
        a.observed
      )
    }
  }

  final case class PropertiesInput(
    name:          Input[NonEmptyString]     = Input.ignore,
    sidereal:      Option[SiderealInput]     = None,
    nonsidereal:   Option[NonsiderealInput]  = None,
    sourceProfile: Input[SourceProfileInput] = Input.ignore
  ) extends EditorInput[Target] {

    override val create: ValidatedInput[Target] =
      (name.notMissing("name"),
       sourceProfile.notMissing("sourceProfile")
      ).tupled.andThen { case (n, sp) =>
        ValidatedInput.requireOne(
          "target",
          sidereal.map(_.createTarget(n, sp)),
          nonsidereal.map(_.createTarget(n, sp))
        )
      }

    override val edit: StateT[EitherInput, Target, Unit] = {
      val validArgs =
        (name.validateIsNotNull("name"),
         sourceProfile.validateIsNotNull("sourceProfile"),
         Validated.condNec(
           sidereal.isEmpty || nonsidereal.isEmpty,
           (),
           InputError.fromMessage("Edit `sidereal` or `nonsidereal` but not both.")
         )
        ).tupled

      for {
        args <- validArgs.liftState
        (n, sp, _) = args
        _ <- Target.name          := n
        _ <- Target.sourceProfile :< sp.map(_.edit)
        _ <- sidereal.map(_.targetEditor).orElse(nonsidereal.map(_.targetEditor)).getOrElse(
          StateT.empty[EitherInput, Target, Unit]
        )
      } yield ()
    }
  }

  object PropertiesInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderInput: Decoder[PropertiesInput] =
      deriveConfiguredDecoder[PropertiesInput]

    implicit val EqTargetInput: Eq[PropertiesInput] =
      Eq.by { a => (
        a.name,
        a.sidereal,
        a.nonsidereal,
        a.sourceProfile
      )}

  }

  final case class CreateInput(
    programId: Program.Id,
    create:    PropertiesInput
  ) {

    val createTarget: StateT[EitherInput, Database, TargetModel] =

      for {
        i <- Database.target.cycleNextUnused
        _ <- Database.program.lookup(programId)
        t  = create.create.map(TargetModel(i, Existence.Present, programId, _, observed = false))
        _ <- Database.target.saveNewIfValid(t)(_.id)
        r <- Database.target.lookup(i)
      } yield r

  }

  object CreateInput {

    def sidereal(
      programId:     Program.Id,
      name:          NonEmptyString,
      input:         SiderealInput,
      sourceProfile: SourceProfileInput
    ): CreateInput =
      CreateInput(programId, PropertiesInput(name.assign, input.some, None, sourceProfile.assign))

    def nonsidereal(
      programId:     Program.Id,
      name:          NonEmptyString,
      input:         NonsiderealInput,
      sourceProfile: SourceProfileInput
    ): CreateInput =
      CreateInput(programId, PropertiesInput(name.assign, None, input.some, sourceProfile.assign))

    implicit val DecoderCreate: Decoder[CreateInput] =
      deriveDecoder[CreateInput]

    implicit val EqCreate: Eq[CreateInput] =
      Eq.by { a => (
        a.programId,
        a.create
      )}
  }

  final case class SelectInput(
    programId:      Option[Program.Id],
    observationId:  Option[Observation.Id],
    observationIds: Option[List[Observation.Id]],
    targetId:       Option[Target.Id],
    targetIds:      Option[List[Target.Id]]
  ) {

    val go: StateT[EitherInput, Database, List[TargetModel]] =
      for {
        p   <- programId.traverse(Database.program.lookup)
        ts0 <- p.traverse(pm => StateT.inspect[EitherInput, Database, List[TargetModel]](_.targets.rows.values.filter(_.programId === pm.id).toList))
        o   <- observationId.traverse(Database.observation.lookup)
        ts1 <- o.map(_.targetEnvironment.asterism.toList).traverse(Database.target.lookupAll)
        os  <- observationIds.traverse(Database.observation.lookupAll)
        ts2 <- os.map(_.flatMap(_.targetEnvironment.asterism.toList)).traverse(Database.target.lookupAll)
        ts3 <- targetId.traverse(Database.target.lookup).map(_.map(t => List(t)))
        ts4 <- targetIds.traverse(Database.target.lookupAll)
      } yield
        (ts0.toList ::: ts1.toList ::: ts2.toList ::: ts3.toList ::: ts4.toList)
          .flatten
          .distinctBy(_.id)
          .sortBy(_.id)

  }

  object SelectInput {

    implicit val DecoderSelect: Decoder[SelectInput] =
      deriveDecoder[SelectInput]

    implicit val EqSelect: Eq[SelectInput] =
      Eq.by { a => (
        a.programId,
        a.observationId,
        a.observationIds,
        a.targetId,
        a.targetIds
      )}

  }

  final case class PatchInput(
    target:    Input[PropertiesInput] = Input.ignore,
    existence: Input[Existence]       = Input.ignore
  ) {

    val editor: StateT[EitherInput, TargetModel, Unit] = {
      val validArgs =
        (existence.validateIsNotNull("existence"),
         target.validateIsNotNull("target")
        ).tupled

      for {
        args <- validArgs.liftState
        (e, _) = args
        _ <- TargetModel.existence := e
        _ <- TargetModel.target    :< target.toOption.map(_.edit)
      } yield ()
    }

  }

  object PatchInput {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[PatchInput] =
      deriveConfiguredDecoder[PatchInput]

    implicit val EqEdit: Eq[PatchInput] =
      Eq.by { a => (
        a.target,
        a.existence
      )}

  }

  final case class EditInput(
    select: SelectInput,
    patch:  PatchInput
  ) {

    val editor: StateT[EitherInput, Database, List[TargetModel]] =
      for {
        ts  <- select.go
        tsʹ <- StateT.liftF[EitherInput, Database, List[TargetModel]](ts.traverse(patch.editor.runS))
        _   <- tsʹ.traverse(t => Database.target.update(t.id, t))
      } yield tsʹ

  }

  object EditInput {

    implicit val DecoderEditInput: Decoder[EditInput] =
      deriveDecoder[EditInput]

    implicit val EqEditInput: Eq[EditInput] =
      Eq.by { a => (
        a.select,
        a.patch
      )}

  }

  final case class TargetEvent (
    id:       Long,
    editType: Event.EditType,
    value:    TargetModel,
  ) extends Event.Edit[TargetModel]

  object TargetEvent {

    def created(value: TargetModel)(id: Long): TargetEvent =
      TargetEvent(id, Event.EditType.Created, value)

    def updated(value: TargetModel)(id: Long): TargetEvent =
      TargetEvent(id, Event.EditType.Updated, value)

  }

}

trait TargetModelOptics { self: TargetModel.type =>

  val id: Lens[TargetModel, Target.Id] =
    Focus[TargetModel](_.id)

  val existence: Lens[TargetModel, Existence] =
    Focus[TargetModel](_.existence)

  val target: Lens[TargetModel, Target] =
    Focus[TargetModel](_.target)

  val name: Lens[TargetModel, NonEmptyString] =
    target.andThen(Target.name)

  val sourceProfile: Lens[TargetModel, SourceProfile] =
    target.andThen(Target.sourceProfile)

  val observed: Lens[TargetModel, Boolean] =
    Focus[TargetModel](_.observed)

}
