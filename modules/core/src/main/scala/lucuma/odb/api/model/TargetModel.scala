// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.json.target._
import lucuma.core.`enum`.{EphemerisKeyType, MagnitudeBand}
import lucuma.core.math.{Coordinates, Declination, Epoch, Parallax, ProperMotion, RadialVelocity, RightAscension}
import lucuma.core.model.{CatalogId, EphemerisKey, Magnitude, SiderealTracking, Target}
import lucuma.core.optics.syntax.optional._
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.TargetEnvironmentModel.SelectTargetEnvironmentInput
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.validatedinput._
import cats.{Eq, Monad}
import cats.data._
import cats.implicits.catsKernelOrderingForOrder
import monocle.Focus
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.util.Enumerated
import monocle.{Lens, Optional}

import scala.collection.immutable.{SortedMap, SortedSet}


final case class TargetModel(
  id:                  Target.Id,
  targetEnvironmentId: TargetEnvironment.Id,
  target:              Target
)

object TargetModel extends TargetOptics {

  implicit val EqTargetModel: Eq[TargetModel] =
    Eq.by { a => (
      a.id,
      a.targetEnvironmentId,
      a.target
    )}

  object parse {

    def ephemerisKey(
      fieldName: String,
      key:       EphemerisKeyType,
      input:     String
    ): ValidatedInput[EphemerisKey] =
      EphemerisKey
        .fromTypeAndDes
        .getOption((key, input))
        .toValidNec(
          InputError.invalidField(fieldName, input, s"Invalid description for ephemeris key type `${key.shortName}`")
        )

  }


  sealed trait TargetOperation extends Product with Serializable

  object TargetOperation {

    case object Create extends TargetOperation
    case object Edit   extends TargetOperation
    case object Delete extends TargetOperation

    implicit val EnumeratedTargetOperation: Enumerated[TargetOperation] =
      Enumerated.of(Create, Edit, Delete)

  }

  /**
   * An ADT describing target edits, which include creating, updating, and
   * deleting.
   */
  final case class TargetEdit(
    op:     TargetOperation,
    target: TargetModel
  )

  object TargetEdit {

    implicit val EqTargetEdit: Eq[TargetEdit] =
      Eq.by { a => (
        a.op,
        a.target
      )}

    def create(t: TargetModel): TargetEdit =
      TargetEdit(TargetOperation.Create, t)

    def edit(t: TargetModel): TargetEdit =
      TargetEdit(TargetOperation.Edit, t)

    def delete(t: TargetModel): TargetEdit =
      TargetEdit(TargetOperation.Delete, t)

  }


  // TODO: move to TargetEnvironmentModel ?


  final case class TargetEnvironmentEdit(
    targetEnvironment: TargetEnvironmentModel,
    observation:       Option[ObservationModel],
    program:           ProgramModel,
    edits:             List[TargetEdit]
  )

  object TargetEnvironmentEdit {

    private def groupAndMap[F[_]: Monad, T](
      db:    DatabaseState[T],
      edits: List[TargetEdit]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      edits
        .groupBy(_.target.targetEnvironmentId)
        .toList
        .traverse { case (vid, edits) =>
          for {
            v <- db.targetEnvironment.lookupValidated(vid)
            o <- v.flatTraverse(_.observationId.traverse(oid => Nested(db.observation.lookupValidated(oid))).value)
            p <- v.flatTraverse(v => db.program.lookupValidated(v.programId))
          } yield (v, o, p).mapN { case (vʹ, oʹ, pʹ) =>
            TargetEnvironmentEdit(vʹ, oʹ, pʹ, edits)
          }
        }.map(_.sequence)

    def fromTargetEdits[F[_]: Monad, T](
      db:    DatabaseState[T],
      edits: F[ValidatedInput[List[TargetEdit]]]
    )(implicit S: Stateful[F, T]):  F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      edits.flatMap(_.flatTraverse(groupAndMap(db, _)))

  }

  /**
   * Input required to create either a non-sidereal or sidereal target.
   */
  final case class CreateTargetInput(
    nonsidereal: Option[CreateNonsiderealInput],
    sidereal:    Option[CreateSiderealInput]
  ) {

    def create[F[_]: Monad, T](
      db:  DatabaseState[T],
      vid: TargetEnvironment.Id
    )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetModel]] =
      ValidatedInput.requireOneF(
        "create",
        nonsidereal.map(_.create(db, vid)),
        sidereal.map(_.create(db, vid))
      )

    def createAll[F[_]: Monad, T](
      db: DatabaseState[T],
      vs: SortedSet[TargetEnvironment.Id]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      ValidatedInput.requireOneF(
        "create",
        nonsidereal.map(_.createAll(db, vs)),
        sidereal.map(_.createAll(db, vs))
      )

  }

  object CreateTargetInput {

    implicit val DecoderCreateTargetInput: Decoder[CreateTargetInput] =
      deriveDecoder[CreateTargetInput]

    implicit val EqCreateTargetInput: Eq[CreateTargetInput] =
      Eq.by { a => (
        a.nonsidereal,
        a.sidereal
      )}

    def nonsidereal(n: CreateNonsiderealInput): CreateTargetInput =
      CreateTargetInput(n.some, None)

    def sidereal(s: CreateSiderealInput): CreateTargetInput =
      CreateTargetInput(None, s.some)

  }

  sealed trait TargetCreator {
    def name: NonEmptyString

    def toGemTarget: ValidatedInput[Target]

    def create[F[_]: Monad, T](
      db:  DatabaseState[T],
      vid: TargetEnvironment.Id
    )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetModel]] =
      for {
        i <- db.target.cycleNextUnused
        v <- db.targetEnvironment.lookupValidated(vid)
        tm = (v, toGemTarget).mapN { (_, g) => TargetModel(i, vid, g) }
        _ <- db.target.saveNewIfValid(tm)(_.id)
      } yield tm

    def createAll[F[_]: Monad, T](
      db: DatabaseState[T],
      vs: SortedSet[TargetEnvironment.Id]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      TargetEnvironmentEdit.fromTargetEdits(
        db,
        SelectTargetEnvironmentInput
          .validateNonEmpty(vs, "cannot create targets without specifying a target environment")
          .flatTraverse {
            _.toNonEmptyList
             .traverse(create(db, _))
             .map(_.sequence.map(_.map(TargetEdit.create).toList))
          }
      )
  }

  /**
   * Describes input used to create a nonsidereal target.
   *
   * @param name    target name
   * @param keyType ephemeris key type
   * @param des     semi-permanent horizons identifier (relative to key type)
   */
  final case class CreateNonsiderealInput(
    name:       NonEmptyString,
    keyType:    EphemerisKeyType,
    des:        String,
    magnitudes: Option[List[MagnitudeModel.Create]]
  ) extends TargetCreator {

    val toEphemerisKey: ValidatedInput[EphemerisKey] =
      parse.ephemerisKey("des", keyType, des)

    override val toGemTarget: ValidatedInput[Target] =
      (toEphemerisKey,
       magnitudes.toList.flatten.traverse(_.toMagnitude)
      ).mapN { (k, ms) =>
        Target(name, Left(k), SortedMap.from(ms.fproductLeft(_.band)))
      }

  }

  object CreateNonsiderealInput {

    implicit val DecoderCreateNonsiderealInput: Decoder[CreateNonsiderealInput] =
      deriveDecoder[CreateNonsiderealInput]

    implicit val EqCreateNonsiderealInput: Eq[CreateNonsiderealInput] =
      Eq.by(cn => (
        cn.name,
        cn.keyType,
        cn.des,
        cn.magnitudes
      ))

  }

  /**
   * Describes input used to create a sidereal target.
   *
   * @param name target name
   * @param ra right ascension coordinate at epoch
   * @param dec declination coordinate at epoch
   * @param epoch time of the base observation
   * @param properMotion proper motion per year in right ascension and declination
   * @param radialVelocity radial velocity
   * @param parallax parallax
   */
  final case class CreateSiderealInput(
    name:           NonEmptyString,
    catalogId:      Option[CatalogIdModel.Input],
    ra:             RightAscensionModel.Input,
    dec:            DeclinationModel.Input,
    epoch:          Option[Epoch],
    properMotion:   Option[ProperMotionModel.Input],
    radialVelocity: Option[RadialVelocityModel.Input],
    parallax:       Option[ParallaxModel.Input],
    magnitudes:     Option[List[MagnitudeModel.Create]]
  ) extends TargetCreator {

    val toSiderealTracking: ValidatedInput[SiderealTracking] =
      (catalogId.traverse(_.toCatalogId),
       ra.toRightAscension,
       dec.toDeclination,
       properMotion.traverse(_.toProperMotion),
       radialVelocity.traverse(_.toRadialVelocity),
       parallax.traverse(_.toParallax)
      ).mapN { (catalogId, ra, dec, pm, rv, px) =>
        SiderealTracking(
          catalogId,
          Coordinates(ra, dec),
          epoch.getOrElse(Epoch.J2000),
          pm,
          rv,
          px
        )
      }

    override val toGemTarget: ValidatedInput[Target] =
      (toSiderealTracking,
       magnitudes.toList.flatten.traverse(_.toMagnitude)
      ).mapN { (pm, ms) =>
        Target(name, Right(pm), SortedMap.from(ms.fproductLeft(_.band)))
      }

  }

  object CreateSiderealInput {

    def fromRaDec(
      name: NonEmptyString,
      ra:   RightAscensionModel.Input,
      dec:  DeclinationModel.Input
    ): CreateSiderealInput =
      CreateSiderealInput(
        name           = name,
        catalogId      = None,
        ra             = ra,
        dec            = dec,
        epoch          = None,
        properMotion   = None,
        radialVelocity = None,
        parallax       = None,
        magnitudes     = None
      )

    implicit val DecoderCreateSiderealInput: Decoder[CreateSiderealInput] =
      deriveDecoder[CreateSiderealInput]

    implicit val EqCreateSidereal: Eq[CreateSiderealInput] =
      Eq.by(cs => (
        cs.name,
        cs.catalogId,
        cs.ra,
        cs.dec,
        cs.epoch,
        cs.properMotion,
        cs.radialVelocity,
        cs.parallax,
        cs.magnitudes
      ))

  }

  //
  // # Target selection.  Choose at least one of `names` or `targetIds`.
  // input SelectTargetInput {
  //
  //   names:     [ NonEmptyString! ]
  //   targetIds: [ TargetId! ]
  //
  // }
  //
  final case class SelectTargetInput(
    names:     Option[List[NonEmptyString]],
    targetIds: Option[List[Target.Id]]
  ) {

    private def toSet[A](as: Option[List[A]]): Set[A] =
      as.fold(Set.empty[A])(_.toSet)

    val nameMatches: Set[NonEmptyString] =
      toSet(names)

    val idMatches: Set[Target.Id] =
      toSet(targetIds)

    def matches(vs: Set[TargetEnvironment.Id], t: TargetModel): Boolean =
      (idMatches(t.id) && (vs.isEmpty || vs(t.targetEnvironmentId))) ||  // identified by target id (env optional)
      (nameMatches(t.target.name) && vs(t.targetEnvironmentId))          // identified by name (requires env)

  }

  object SelectTargetInput {

    implicit val DecoderSelectTarget: Decoder[SelectTargetInput] =
      deriveDecoder[SelectTargetInput]

    implicit val EqSelectTarget: Eq[SelectTargetInput] =
      Eq.by { a => (
        a.names,
        a.targetIds
      )}

  }

  sealed trait TargetEditor {

    def select: SelectTargetInput

    def editor: ValidatedInput[State[Target, Unit]]

    private def editTargets[F[_]: Monad, T](
      db: DatabaseState[T],
      vs: SortedSet[TargetEnvironment.Id]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetModel]]] =
      for {
        v   <- vs.toList.traverse(v => db.targetEnvironment.lookupValidated[F](v)).map(_.sequence)
        tms <- db.target.findAll { case (_, tm) => select.matches(vs, tm) }
        tmsʹ = (v, editor).mapN { (_, e) =>
          tms.map(target.modify(t => e.runS(t).value))
        }
        _ <- tmsʹ.traverse(_.traverse(tm => db.target.update(tm.id, tm)))
      } yield tmsʹ

    def edit[F[_]: Monad, T](
      db: DatabaseState[T],
      vs: SortedSet[TargetEnvironment.Id]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      TargetEnvironmentEdit.fromTargetEdits(
        db,
        Nested(editTargets(db, vs)).map(_.map(TargetEdit.edit)).value
      )

  }

  final case class EditNonsiderealInput(
    select: SelectTargetInput,
    name:   Input[NonEmptyString] = Input.ignore,
    key:    Input[EphemerisKey]   = Input.ignore,
  ) extends TargetEditor {

    override val editor: ValidatedInput[State[Target, Unit]] =
      (name.validateIsNotNull("name"),
       key.validateIsNotNull("key")
      ).mapN { case (n, k) =>
        for {
          _ <- TargetModel.name         := n
          _ <- TargetModel.ephemerisKey := k
        } yield ()
      }

  }

  object EditNonsiderealInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEditNonSidereal: Decoder[EditNonsiderealInput] =
      deriveConfiguredDecoder[EditNonsiderealInput]

    implicit val EqEditNonsidereal: Eq[EditNonsiderealInput] =
      Eq.by(en => (
        en.select,
        en.name,
        en.key
      ))

  }

  final case class EditSiderealInput(
    select:           SelectTargetInput,
    name:             Input[NonEmptyString]            = Input.ignore,
    catalogId:        Input[CatalogIdModel.Input]      = Input.ignore,
    ra:               Input[RightAscensionModel.Input] = Input.ignore,
    dec:              Input[DeclinationModel.Input]    = Input.ignore,
    epoch:            Input[Epoch]                     = Input.ignore,
    properMotion:     Input[ProperMotionModel.Input]   = Input.ignore,
    radialVelocity:   Input[RadialVelocityModel.Input] = Input.ignore,
    parallax:         Input[ParallaxModel.Input]       = Input.ignore,
    magnitudes:       Option[MagnitudeModel.EditList],
  ) extends TargetEditor {

    override val editor: ValidatedInput[State[Target, Unit]] =
      (name          .validateIsNotNull("name"),
       catalogId     .validateNullable(_.toCatalogId),
       ra            .validateNotNullable("ra")(_.toRightAscension),
       dec           .validateNotNullable("dec")(_.toDeclination),
       epoch         .validateIsNotNull("epoch"),
       properMotion  .validateNullable(_.toProperMotion),
       radialVelocity.validateNullable(_.toRadialVelocity),
       parallax      .validateNullable(_.toParallax),
       magnitudes    .traverse(_.editor)
      ).mapN { (name, catalogId, ra, dec, epoch, pm, rv, px, ms) =>
        for {
          _ <- TargetModel.name           := name
          _ <- TargetModel.catalogId      := catalogId
          _ <- TargetModel.ra             := ra
          _ <- TargetModel.dec            := dec
          _ <- TargetModel.epoch          := epoch
          _ <- TargetModel.properMotion   := pm
          _ <- TargetModel.radialVelocity := rv
          _ <- TargetModel.parallax       := px
          _ <- TargetModel.magnitudes.mod(m => ms.fold(m)(_.runS(m).value))
        } yield ()
      }

  }

  object EditSiderealInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults


    implicit val DecoderEditSidereal: Decoder[EditSiderealInput] =
      deriveConfiguredDecoder[EditSiderealInput]

    implicit val EqEditSidereal: Eq[EditSiderealInput] =
      Eq.by(es => (
        es.select,
        es.name,
        es.catalogId,
        es.ra,
        es.dec,
        es.epoch,
        es.properMotion,
        es.radialVelocity,
        es.parallax,
        es.magnitudes
      ))

  }


  sealed trait EditTargetAction {
    def addSidereal:     Option[CreateSiderealInput]

    def addNonsidereal:  Option[CreateNonsiderealInput]

    def editSidereal:    Option[EditSiderealInput]

    def editNonsidereal: Option[EditNonsiderealInput]

    def delete:          Option[SelectTargetInput]

    protected def doDeletion[F[_]: Monad, T](
      db:  DatabaseState[T],
      vs:  SortedSet[TargetEnvironment.Id],
      del: SelectTargetInput
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      TargetEnvironmentEdit.fromTargetEdits(
        db,
        for {
          v   <- vs.toList.traverse(v => db.targetEnvironment.lookupValidated[F](v)).map(_.sequence)
          tms <- db.target.findAll { case (_, tm) => del.matches(vs, tm) }
          res <- v.as(tms.traverse(tm => db.target.delete(tm.id)).as(tms)).sequence
        } yield Nested(res).map(TargetEdit.delete).value
      )

    def editEnv[F[_]: Monad, T](
      db: DatabaseState[T],
      vs: SortedSet[TargetEnvironment.Id]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      ValidatedInput.requireOneF("edit",
        addSidereal.map(_.createAll[F, T](db, vs)),
        addNonsidereal.map(_.createAll[F, T](db, vs)),
        editSidereal.map(_.edit[F, T](db, vs)),
        delete.map(doDeletion[F, T](db, vs, _))
      )
  }


  final case class BulkEditTargetInput(
    select:          Option[SelectTargetEnvironmentInput],

    addSidereal:     Option[CreateSiderealInput],
    addNonsidereal:  Option[CreateNonsiderealInput],
    editSidereal:    Option[EditSiderealInput],
    editNonsidereal: Option[EditNonsiderealInput],
    delete:          Option[SelectTargetInput]
  ) extends EditTargetAction {

    def edit[F[_]: Monad, T](
      db: DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      for {
        s <- SelectTargetEnvironmentInput.ids(db, select)
        e <- s.traverse(editEnv(db, _))
      } yield e.flatten

  }

  object BulkEditTargetInput {

    implicit val DecoderBulkEditTargetInput: Decoder[BulkEditTargetInput] =
      deriveDecoder[BulkEditTargetInput]

    implicit val EqBulkEditTarget: Eq[BulkEditTargetInput] =
      Eq.by { a => (
        a.addSidereal,
        a.addNonsidereal,
        a.editSidereal,
        a.editNonsidereal,
        a.delete
      )}

    val Empty: BulkEditTargetInput =
      BulkEditTargetInput(None, None, None, None, None, None)

    def addSidereal(s: Option[SelectTargetEnvironmentInput], c: CreateSiderealInput): BulkEditTargetInput =
      Empty.copy(select = s, addSidereal = c.some)

    def addNonsidereal(s: Option[SelectTargetEnvironmentInput], c: CreateNonsiderealInput): BulkEditTargetInput =
      Empty.copy(select = s, addNonsidereal = c.some)

    def editSidereal(s: Option[SelectTargetEnvironmentInput], e: EditSiderealInput): BulkEditTargetInput =
      Empty.copy(select = s, editSidereal = e.some)

    def editNonsidereal(s: Option[SelectTargetEnvironmentInput], e: EditNonsiderealInput): BulkEditTargetInput =
      Empty.copy(select = s, editNonsidereal = e.some)

    def delete(s: Option[SelectTargetEnvironmentInput], d: SelectTargetInput): BulkEditTargetInput =
      Empty.copy(select = s, delete = d.some)

  }

  final case class EditTargetInput(
    addSidereal:     Option[CreateSiderealInput],
    addNonsidereal:  Option[CreateNonsiderealInput],
    editSidereal:    Option[EditSiderealInput],
    editNonsidereal: Option[EditNonsiderealInput],
    delete:          Option[SelectTargetInput]
  ) extends EditTargetAction

  object EditTargetInput {

    implicit val DecoderEditTargetInput: Decoder[EditTargetInput] =
      deriveDecoder[EditTargetInput]

    implicit val EqEditTargetInput: Eq[EditTargetInput] =
      Eq.by { a => (
        a.addSidereal,
        a.addNonsidereal,
        a.editSidereal,
        a.editNonsidereal,
        a.delete
      )}

    val Empty: EditTargetInput =
      EditTargetInput(None, None, None, None, None)

    def addSidereal(c: CreateSiderealInput): EditTargetInput =
      Empty.copy(addSidereal = c.some)

    def addNonsidereal(c: CreateNonsiderealInput): EditTargetInput =
      Empty.copy(addNonsidereal = c.some)

    def editSidereal(e: EditSiderealInput): EditTargetInput =
      Empty.copy(editSidereal = e.some)

    def editNonsidereal(e: EditNonsiderealInput): EditTargetInput =
      Empty.copy(editNonsidereal = e.some)

    def delete(d: SelectTargetInput): EditTargetInput =
      Empty.copy(delete = d.some)

  }


  final case class BulkEditTargetListInput(
    select: Option[SelectTargetEnvironmentInput],
    edits:  List[EditTargetInput]
  ) {

    def edit[F[_]: Monad, T](
      db:  DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] =
      for {
        s <- SelectTargetEnvironmentInput.ids(db, select)
        e <- s.traverse(ids => edits.traverse(_.editEnv(db, ids))).map(_.map(_.flatSequence))
      } yield e.flatten

  }

  object BulkEditTargetListInput {

    implicit val DecoderBulkEditTargetListInput: Decoder[BulkEditTargetListInput] =
      deriveDecoder[BulkEditTargetListInput]

    implicit val EqBulkEditTargetListInput: Eq[BulkEditTargetListInput] =
      Eq.by { a => (
        a.select,
        a.edits
      )}

  }


  final case class BulkReplaceTargetListInput(
    select:  SelectTargetEnvironmentInput,
    replace: List[CreateTargetInput]
  ) {

    def replace[F[_]: Monad, T](
      db:  DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentEdit]]] = {

      def deleteTargets(vs: SortedSet[TargetEnvironment.Id]): F[List[TargetEdit]] =
        for {
          tms <- db.target.findAll { case (_, tm) => vs(tm.targetEnvironmentId) }
          res <- tms.traverse(tm => db.target.delete(tm.id)).as(tms.map(TargetEdit.delete))
        } yield res

      def createTargets(vs: SortedSet[TargetEnvironment.Id]): F[ValidatedInput[List[TargetEdit]]] =
        replace
          .traverse(cti => vs.toList.traverse(cti.create(db, _)))
          .map(_.flatten.sequence.map(_.map(TargetEdit.create)))

      TargetEnvironmentEdit.fromTargetEdits(
        db,
        for {
          s <- select.selectIds(db)
          d <- s.traverse(deleteTargets)
          e <- s.traverse(createTargets)
        } yield (d, e.flatten).mapN(_ ++ _)
      )
    }

  }

  object BulkReplaceTargetListInput {

    implicit val DecoderBulkReplaceTargetListInput: Decoder[BulkReplaceTargetListInput] =
      deriveDecoder[BulkReplaceTargetListInput]

    implicit val EqBulkReplaceTargetListInput: Eq[BulkReplaceTargetListInput] =
      Eq.by { a => (
        a.select,
        a.replace
      )}

  }

}


trait TargetOptics { self: TargetModel.type =>

  val target: Lens[TargetModel, Target] =
    Focus[TargetModel](_.target)

  val name: Lens[Target, NonEmptyString] =
    Target.name

  val nonsiderealTarget: Optional[Target, Target] =
    Optional.filter[Target](_.track.isLeft)

  val siderealTarget: Optional[Target, Target] =
    Optional.filter[Target](_.track.isRight)

  val ephemerisKey: Optional[Target, EphemerisKey] =
    Target.track.andThen(monocle.std.either.stdLeft[EphemerisKey, SiderealTracking])

  val siderealTracking: Optional[Target, SiderealTracking] =
    Target.track.andThen(monocle.std.either.stdRight[EphemerisKey, SiderealTracking])

  val catalogId: Optional[Target, Option[CatalogId]] =
    siderealTracking.andThen(SiderealTracking.catalogId)

  val coordinates: Optional[Target, Coordinates] =
    siderealTracking.andThen(SiderealTracking.baseCoordinates)

  val ra: Optional[Target, RightAscension] =
    coordinates.andThen(Coordinates.rightAscension)

  val dec: Optional[Target, Declination] =
    coordinates.andThen(Coordinates.declination)

  val epoch: Optional[Target, Epoch] =
    siderealTracking.andThen(SiderealTracking.epoch)

  val properMotion: Optional[Target, Option[ProperMotion]] =
    siderealTracking.andThen(SiderealTracking.properMotion)

  val radialVelocity: Optional[Target, Option[RadialVelocity]] =
    siderealTracking.andThen(SiderealTracking.radialVelocity)

  val parallax: Optional[Target, Option[Parallax]] =
    siderealTracking.andThen(SiderealTracking.parallax)

  val magnitudes: Lens[Target, SortedMap[MagnitudeBand, Magnitude]] =
    Target.magnitudes

}
