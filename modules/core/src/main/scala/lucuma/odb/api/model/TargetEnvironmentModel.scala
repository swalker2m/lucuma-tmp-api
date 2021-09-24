// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Coordinates
import lucuma.core.model.{Observation, Program, Target, WithId}
import lucuma.core.optics.syntax.lens._
import lucuma.odb.api.model.syntax.input._
import cats.{Eq, Monad}
import cats.data.{NonEmptySet, State}
import cats.implicits.catsKernelOrderingForOrder
import cats.mtl.Stateful
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.auto._
import eu.timepit.refined.cats.refTypeOrder
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Lens

import scala.collection.immutable.SortedSet


final case class TargetEnvironment(
  explicitBase: Option[Coordinates],
  science:      Set[Target]
)

object TargetEnvironment extends WithId('v') {

  implicit val EqTargetEnvironment: Eq[TargetEnvironment] =
    Eq.by { a => (
      a.explicitBase,
      a.science
    )}

}


final case class TargetEnvironmentModel(
  id:            TargetEnvironment.Id,
  programId:     Program.Id,
  observationId: Option[Observation.Id],

  explicitBase:  Option[Coordinates]
) {

  def toTargetEnvironment(
    science: Iterable[Target]
  ): TargetEnvironment =
    TargetEnvironment(explicitBase, Set.from(science))
}

object TargetEnvironmentModel extends TargetEnvironmentModelOptics {

  def empty(
    id:            TargetEnvironment.Id,
    programId:     Program.Id,
    observationId: Option[Observation.Id]
  ): TargetEnvironmentModel =
    TargetEnvironmentModel(id, programId, observationId, None)

  implicit val EqTargetEnvironmentModel: Eq[TargetEnvironmentModel] =
    Eq.by { tem => (
      tem.id,
      tem.programId,
      tem.observationId,
      tem.explicitBase
    )}

  final case class Group[A](
    value: A,
    targetEnvironmentIds: SortedSet[TargetEnvironment.Id]
  )

  object Group {

    implicit def EqGroup[A: Eq]: Eq[Group[A]] =
      Eq.by { tem => (
        tem.value,
        tem.targetEnvironmentIds
      )}

  }

  final case class SelectTargetEnvironmentInput(
    all:                Option[Program.Id],
    program:            Option[Program.Id],
    observations:       Option[List[Observation.Id]],
    targetEnvironments: Option[List[TargetEnvironment.Id]]
  ) {

    private val obsList: List[Observation.Id]          = observations.toList.flatten
    private val envList: List[TargetEnvironment.Id]    = targetEnvironments.toList.flatten
    private val allIncludes: Set[Program.Id]           = all.toSet
    private val prgIncludes: Set[Program.Id]           = program.toSet
    private val obsIncludes: Set[Observation.Id]       = obsList.toSet
    private val envIncludes: Set[TargetEnvironment.Id] = envList.toSet

    private def toSelected(sel: ValidatedInput[List[TargetEnvironmentModel]]): ValidatedInput[List[TargetEnvironmentModel]] =
      sel.getOrElse(List.empty[TargetEnvironmentModel]).map(_.programId).distinct match {
        case Nil      => sel
        case _ :: Nil => sel
        case pids     => InputError.fromMessage(s"Multiple program selected: ${pids.mkString(",")}").invalidNec[List[TargetEnvironmentModel]]
      }

    def select[F[_] : Monad, T](
      db: DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentModel]]] =

      for {
        a   <- db.program.lookupAllValidated(all.toList)
        p   <- db.program.lookupAllValidated(program.toList)
        os  <- db.observation.lookupAllValidated(obsList)
        vs  <- db.targetEnvironment.lookupAllValidated(envList)
        sel <- db.targetEnvironment.findAll { case (vid, v) =>
          envIncludes(vid) ||                                     // explicitly listed
            v.observationId.exists(obsIncludes) ||                // observation listed
            allIncludes(v.programId) ||                           // all for this program
            (v.observationId.isEmpty && prgIncludes(v.programId)) // associated with program but not an obs
        }
      } yield toSelected((a, p, os, vs).tupled.as(sel))

    def selectIds[F[_]: Monad, T](
      db: DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[SortedSet[TargetEnvironment.Id]]] =
      select(db).map(_.map(lst => SortedSet.from(lst.map(_.id))))

  }


  object SelectTargetEnvironmentInput {

    implicit val DecoderSelectTargetEnvironment: Decoder[SelectTargetEnvironmentInput] =
      deriveDecoder[SelectTargetEnvironmentInput]

    implicit val EqSelectTargetEnvironment: Eq[SelectTargetEnvironmentInput] =
      Eq.by { a => (
        a.all,
        a.program,
        a.observations,
        a.targetEnvironments
      )}

    def invalid(msg: String): ValidatedInput[NonEmptySet[TargetEnvironment.Id]] =
      InputError.fromMessage(s"No target environment was selected: $msg").invalidNec[NonEmptySet[TargetEnvironment.Id]]

    def validateNonEmpty(
      sel: SortedSet[TargetEnvironment.Id],
      msg: => String
    ): ValidatedInput[NonEmptySet[TargetEnvironment.Id]] =
      sel
        .headOption
        .fold(invalid(msg)) { h =>
          NonEmptySet(h, sel.tail).validNec[InputError]
        }

    def ids[F[_]: Monad, T](
      db: DatabaseState[T],
      sel: Option[SelectTargetEnvironmentInput]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[SortedSet[TargetEnvironment.Id]]] =
      sel
        .traverse(_.selectIds(db))
        .map(_.sequence.map(_.getOrElse(SortedSet.empty[TargetEnvironment.Id])))

  }

  final case class CreateTargetEnvironmentInput(
    explicitBase: Option[CoordinatesModel.Input],
    science:      Option[List[TargetModel.CreateTargetInput]]
  ) {

    // TODO: nothing stops you from creating one for an observation where one
    // TODO: already exists

    def create[F[_]: Monad, T](
      db:  DatabaseState[T],
      pid: Program.Id,
      oid: Option[Observation.Id],
    )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetEnvironmentModel]] =
      for {
        i <- db.targetEnvironment.cycleNextUnused
        p <- db.program.lookupValidated(pid)
        o <- oid.traverse(o => db.observation.lookupValidated(o)).map(_.sequence)
        b  = explicitBase.traverse(_.toCoordinates)
        s <- science.toList.flatten.traverse(_.createAll(db, SortedSet(i))).map(_.flatSequence)
        t  = (p, o, b, s).mapN { (_, _, bʹ, _) =>
          TargetEnvironmentModel(i, pid, oid, bʹ)
        }
        _ <- db.targetEnvironment.saveNewIfValid(t)(_.id)
      } yield t

  }

  object CreateTargetEnvironmentInput {

    implicit val DecoderCreate: Decoder[CreateTargetEnvironmentInput] =
      deriveDecoder[CreateTargetEnvironmentInput]

    implicit val EqCreate: Eq[CreateTargetEnvironmentInput] =
      Eq.by { a => (
        a.explicitBase,
        a.science
      )}

    def single(science: TargetModel.CreateTargetInput): CreateTargetEnvironmentInput =
      CreateTargetEnvironmentInput(None, List(science).some)

    def singleNonsidereal(nonsidereal: TargetModel.CreateNonsiderealInput): CreateTargetEnvironmentInput =
      CreateTargetEnvironmentInput(None, List(TargetModel.CreateTargetInput.nonsidereal(nonsidereal)).some)

    def singleSidereal(sidereal: TargetModel.CreateSiderealInput): CreateTargetEnvironmentInput =
      CreateTargetEnvironmentInput(None, List(TargetModel.CreateTargetInput.sidereal(sidereal)).some)

    def fromSidereal(cs: IterableOnce[TargetModel.CreateSiderealInput]): CreateTargetEnvironmentInput =
      CreateTargetEnvironmentInput(None, cs.iterator.map(TargetModel.CreateTargetInput.sidereal).toList.some)

    def fromNonsidereal(cs: IterableOnce[TargetModel.CreateNonsiderealInput]): CreateTargetEnvironmentInput =
      CreateTargetEnvironmentInput(None, cs.iterator.map(TargetModel.CreateTargetInput.nonsidereal).toList.some)

  }

  final case class BulkEditTargetEnvironmentInput(
    select:       SelectTargetEnvironmentInput,
    explicitBase: Input[CoordinatesModel.Input] = Input.ignore
  ) {

    val editor: ValidatedInput[State[TargetEnvironmentModel, Unit]] =
      explicitBase.validateNullable(_.toCoordinates).map { b =>
        (TargetEnvironmentModel.explicitBase := b).void
      }

    def edit[F[_]: Monad, T](
      db: DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentModel]]] =
      for {
        vs <- select.select(db)
        r   = (vs, editor).mapN { (v, ed) =>
          v.map(ed.runS).map(_.value)
        }
        _ <- r.traverse_(tms => tms.traverse_(tm => db.targetEnvironment.update(tm.id, tm)))
      } yield r

  }

  object EditTargetEnvironmentInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[BulkEditTargetEnvironmentInput] =
      deriveConfiguredDecoder[BulkEditTargetEnvironmentInput]

    implicit val EqEdit: Eq[BulkEditTargetEnvironmentInput] =
      Eq.by { a => (
        a.select,
        a.explicitBase
      )}

    def explicitBase(
      select: SelectTargetEnvironmentInput,
      ra:     RightAscensionModel.Input,
      dec:    DeclinationModel.Input
    ): BulkEditTargetEnvironmentInput =
      BulkEditTargetEnvironmentInput(
        select,
        Input.assign(CoordinatesModel.Input(ra, dec))
      )

  }
}



trait TargetEnvironmentModelOptics { self: TargetEnvironmentModel.type =>

  val explicitBase: Lens[TargetEnvironmentModel, Option[Coordinates]] =
    Lens[TargetEnvironmentModel, Option[Coordinates]](_.explicitBase)(a => _.copy(explicitBase = a))

}
