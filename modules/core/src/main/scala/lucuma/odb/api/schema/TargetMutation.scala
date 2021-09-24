// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.MonadError
import cats.effect.std.Dispatcher
import lucuma.odb.api.model.{CatalogIdModel, CoordinatesModel, DeclinationModel, MagnitudeModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.model.TargetModel.{TargetEdit, TargetEnvironmentEdit, TargetOperation}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.core.`enum`.MagnitudeSystem
import cats.syntax.option._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


trait TargetMutation extends TargetScalars {

  import context._
  import GeneralSchema.NonEmptyStringType
  import NumericUnitsSchema._
  import ObservationSchema.{ObservationIdType, ObservationType}
  import ProgramSchema.{ProgramIdType, ProgramType}
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyTypeEnumType, EnumTypeMagnitudeBand, EnumTypeMagnitudeSystem, TargetEnvironmentIdType, TargetEnvironmentModelType, TargetIdType, TargetModelType}

  import syntax.inputtype._
  import syntax.inputobjecttype._

  implicit val EnumTypeTargetOperation: EnumType[TargetOperation] =
    EnumType.fromEnumerated(
      "TargetOperation",
      "The target edit that was performed"
    )

  def TargetEditType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], TargetEdit] =
    ObjectType(
      name     = "TargetEdit",
      fieldsFn = () => fields(

        Field(
          name        = "op",
          fieldType   = EnumTypeTargetOperation,
          description = "Which operation was performed".some,
          resolve     = _.value.op
        ),

        Field(
          name        = "target",
          fieldType   = TargetModelType[F],
          description = "Target that was edited".some,
          resolve     = _.value.target
        )

      )
    )

  def TargetEnvironmentEditType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], TargetEnvironmentEdit] =
    ObjectType(
      name     = "TargetEnvironmentEdit",
      fieldsFn = () => fields(

        Field(
          name        = "targetEnvironment",
          fieldType   = TargetEnvironmentModelType[F],
          description = "Target environment that was edited".some,
          resolve     = _.value.targetEnvironment
        ),

        Field(
          name        = "observation",
          fieldType   = OptionType(ObservationType[F]),
          description = "Observation that houses the target environment, if any".some,
          resolve     = _.value.observation
        ),

        Field(
          name        = "program",
          fieldType   = ProgramType[F],
          description = "Program that houses the target environment".some,
          resolve     = _.value.program
        ),

        Field(
          name        = "edits",
          fieldType   = ListType(TargetEditType[F]),
          description = "Details any edits that were performed".some,
          resolve     = _.value.edits
        )
      )
    )


  implicit val EnumTypeDeclinationUnits: EnumType[DeclinationModel.Units] =
    EnumType.fromEnumerated(
      "DeclinationUnits",
      "Unit options for Declination values"
    )

  implicit val EnumTypeRightAscensionUnits: EnumType[RightAscensionModel.Units] =
    EnumType.fromEnumerated(
      "RightAscensionUnits",
      "Unit options for RightAscension values"
    )

  implicit val EnumTypeProperMotionUnits: EnumType[ProperMotionModel.Units] =
    EnumType.fromEnumerated(
      "ProperMotionComponentUnits",
      "Unit options for proper motion components (RA and Dec)"
    )

  implicit val EnumTypeRadialVelocityUnits: EnumType[RadialVelocityModel.Units] =
    EnumType.fromEnumerated(
      "RadialVelocityUnits",
      "Unit options for radial velocity values"
    )

  implicit val EnumTypeParallaxUnits: EnumType[ParallaxModel.Units] =
    EnumType.fromEnumerated(
      "ParallaxUnits",
      "Unit options for parallax values"
    )

  implicit val InputObjectCatalogId: InputObjectType[CatalogIdModel.Input] =
    deriveInputObjectType[CatalogIdModel.Input](
      InputObjectTypeName("CatalogIdInput"),
      InputObjectTypeDescription("Catalog id consisting of catalog name and string identifier")
    )

  implicit val InputObjectTypeCoordinates: InputObjectType[CoordinatesModel.Input] =
    deriveInputObjectType[CoordinatesModel.Input](
      InputObjectTypeName("CoordinatesInput"),
      InputObjectTypeDescription("Absolute coordinates relative base epoch")
    )

  implicit val InputObjectDeclination: InputObjectType[DeclinationModel.Input] =
    deriveInputObjectType[DeclinationModel.Input](
      InputObjectTypeName("DeclinationInput"),
      InputObjectTypeDescription("Declination, choose one of the available units")
    )

  implicit val InputObjectRightAscension: InputObjectType[RightAscensionModel.Input] =
    deriveInputObjectType[RightAscensionModel.Input](
      InputObjectTypeName("RightAscensionInput"),
      InputObjectTypeDescription("Right Ascension, choose one of the available units")
    )

  implicit val InputObjectProperMotionComponent: InputObjectType[ProperMotionModel.ComponentInput] =
    deriveInputObjectType[ProperMotionModel.ComponentInput](
      InputObjectTypeName("ProperMotionComponentInput"),
      InputObjectTypeDescription(s"Proper motion component, choose one of the available units")
    )

  implicit val InputObjectProperMotion: InputObjectType[ProperMotionModel.Input] =
    deriveInputObjectType[ProperMotionModel.Input](
      InputObjectTypeName("ProperMotionInput"),
      InputObjectTypeDescription("Proper motion, choose one of the available units")
    )

  implicit val InputObjectRadialVelocity: InputObjectType[RadialVelocityModel.Input] =
    deriveInputObjectType[RadialVelocityModel.Input](
      InputObjectTypeName("RadialVelocityInput"),
      InputObjectTypeDescription("Radial velocity, choose one of the available units")
    )

  implicit val InputObjectParallax: InputObjectType[ParallaxModel.Input] =
    deriveInputObjectType[ParallaxModel.Input](
      InputObjectTypeName("ParallaxModelInput"),
      InputObjectTypeDescription("Parallax, choose one of the available units")
    )

  implicit val InputObjectMagnitudeCreate: InputObjectType[MagnitudeModel.Create] =
    deriveInputObjectType[MagnitudeModel.Create](
      InputObjectTypeName("MagnitudeCreateInput"),
      InputObjectTypeDescription("Magnitude creation parameters"),
      ReplaceInputField(
        "system",
        InputField(
          name         = "system",
          fieldType    = OptionInputType(EnumTypeMagnitudeSystem),
          defaultValue = Some(MagnitudeSystem.Vega: MagnitudeSystem)
        )
      )
    )

  implicit val InputObjectMagnitudeEdit: InputObjectType[MagnitudeModel.Edit] =
    deriveInputObjectType[MagnitudeModel.Edit](
      InputObjectTypeName("MagnitudeEditInput"),
      InputObjectTypeDescription("Magnitude editing parameters"),
      ReplaceInputField("value",  BigDecimalType.notNullableField("value")          ),
      ReplaceInputField("system", EnumTypeMagnitudeSystem.notNullableField("system")),
      ReplaceInputField("error",  BigDecimalType.nullableField("error")             )
    )

  implicit val InputObjectMagnitudeEditAction: InputObjectType[MagnitudeModel.EditAction] =
    deriveInputObjectType[MagnitudeModel.EditAction](
      InputObjectTypeName("MagnitudeEditAction"),
      InputObjectTypeDescription("Magnitude edit action (choose one option only)")
    )

  implicit val InputObjectMagnitudeEditList: InputObjectType[MagnitudeModel.EditList] =
    deriveInputObjectType[MagnitudeModel.EditList](
      InputObjectTypeName("MagnitudeEditList"),
      InputObjectTypeDescription("Magnitude list editing (choose one option only)")
    )

  implicit val InputObjectTypeCreateNonsidereal: InputObjectType[TargetModel.CreateNonsiderealInput] =
    deriveInputObjectType[TargetModel.CreateNonsiderealInput](
      InputObjectTypeName("CreateNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target parameters")
    )

  implicit val InputObjectTypeCreateSidereal: InputObjectType[TargetModel.CreateSiderealInput] =
    deriveInputObjectType[TargetModel.CreateSiderealInput](
      InputObjectTypeName("CreateSiderealInput"),
      InputObjectTypeDescription("Sidereal target parameters")
    )

//  val ArgumentTargetCreateNonsidereal: Argument[TargetModel.CreateNonsiderealInput] =
//    InputObjectTypeCreateNonsidereal.argument(
//      "input",
//      "Nonsidereal target description"
//    )

//  val ArgumentTargetCreateSidereal: Argument[TargetModel.CreateSidereal] =
//    InputObjectTypeCreateSidereal.argument(
//      "input",
//      "Sidereal target description"
//    )

  implicit val InputObjectTypeCreateTarget: InputObjectType[TargetModel.CreateTargetInput] =
    deriveInputObjectType[TargetModel.CreateTargetInput](
      InputObjectTypeName("CreateTargetInput"),
      InputObjectTypeDescription("Target creation parameters")
    )

//  val ArgumentTargetCreate: Argument[TargetModel.Create] =
//    InputObjectTypeCreateTarget.argument(
//      "input",
//      "Target creation parameters.  Choose 'nonSidereal' or 'sidereal'."
//    )

  implicit val InputObjectTypeSelectTarget: InputObjectType[TargetModel.SelectTargetInput] =
    deriveInputObjectType[TargetModel.SelectTargetInput](
      InputObjectTypeName("SelectTargetInput"),
      InputObjectTypeDescription("Target selection parameters.  Choose at least one of `names` or `targetIds`.")
    )

  implicit val InputObjectTypeEditNonsidereal: InputObjectType[TargetModel.EditNonsiderealInput] =
    deriveInputObjectType[TargetModel.EditNonsiderealInput](
      InputObjectTypeName("EditNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target edit parameters"),

      ReplaceInputField("name", NonEmptyStringType.notNullableField("name")),
      ReplaceInputField("key",  EphemerisKeyType  .notNullableField("key"))
    )


  implicit val InputObjectTypeTargetEditSidereal: InputObjectType[TargetModel.EditSiderealInput] =
    deriveInputObjectType[TargetModel.EditSiderealInput](
      InputObjectTypeName("EditSiderealInput"),
      InputObjectTypeDescription("Sidereal target edit parameters"),

      DocumentInputField("magnitudes",    "Edit magnitudes"                                               ),

      ReplaceInputField("name",           NonEmptyStringType       .notNullableField("name"       )),
      ReplaceInputField("catalogId",      InputObjectCatalogId     .nullableField("catalogId"     )),
      ReplaceInputField("ra",             InputObjectRightAscension.notNullableField("ra"         )),
      ReplaceInputField("dec",            InputObjectDeclination   .notNullableField("dec"        )),
      ReplaceInputField("epoch",          EpochStringType          .notNullableField("epoch"      )),
      ReplaceInputField("properMotion",   InputObjectProperMotion  .nullableField("properMotion"  )),
      ReplaceInputField("radialVelocity", InputObjectRadialVelocity.nullableField("radialVelocity")),
      ReplaceInputField("parallax",       InputObjectParallax      .nullableField("parallax"      ))
    )

//  val ArgumentTargetEditSidereal: Argument[TargetModel.EditSidereal] =
//    InputObjectTypeTargetEditSidereal.argument(
//      "input",
//      "Sidereal target edit"
//    )

  implicit val InputObjectTypeSelectTargetEnvironmentInput: InputObjectType[TargetEnvironmentModel.SelectTargetEnvironmentInput] =
    deriveInputObjectType[TargetEnvironmentModel.SelectTargetEnvironmentInput](
      InputObjectTypeName("SelectTargetEnvironmentInput"),
      InputObjectTypeDescription("Target environment selection parameters. Choose at least one option.")
    )

  implicit val InputObjectTypeBulkEditTargetInput: InputObjectType[TargetModel.BulkEditTargetInput] =
    deriveInputObjectType[TargetModel.BulkEditTargetInput](
      InputObjectTypeName("BulkEditTargetInput"),
      InputObjectTypeDescription("Target editing parameters")
    )

  val ArgumentBulkEditTargetInput: Argument[TargetModel.BulkEditTargetInput] =
    InputObjectTypeBulkEditTargetInput.argument(
      "input",
      "Target edit"
    )

//  implicit val InputObjectTypeTargetEditAction: InputObjectType[TargetModel.EditTargetAction] =
//    deriveInputObjectType[TargetModel.EditTargetAction](
//      InputObjectTypeName("EditTargetActionInput"),
//      InputObjectTypeDescription("Target edit action (choose one of 'add', 'delete', or 'edit'.")
//    )
//
//  implicit val InputObjectTypeTargetEditList: InputObjectType[TargetModel.EditTargetList] =
//    deriveInputObjectType[TargetModel.EditTargetList](
//      InputObjectTypeName("EditTargetListInput"),
//      InputObjectTypeDescription("Target list edit input (choose one of 'replaceList' or 'editList'.")
//    )

  implicit val InputObjectTypeTargetEnvironmentCreate: InputObjectType[TargetEnvironmentModel.CreateTargetEnvironmentInput] =
    deriveInputObjectType[TargetEnvironmentModel.CreateTargetEnvironmentInput](
      InputObjectTypeName("CreateTargetEnvironmentInput"),
      InputObjectTypeDescription("Target environment creation input parameters")
    )

//  implicit val InputObjectTypeTargetEnvironmentEdit: InputObjectType[TargetEnvironmentModel.EditTargetEnvironmentInput] =
//    deriveInputObjectType[TargetEnvironmentModel.EditTargetEnvironmentInput](
//      InputObjectTypeName("EditTargetEnvironmentInput"),
//      InputObjectTypeDescription("Target environment edit input parameters"),
//
//      ReplaceInputField("explicitBase", InputObjectTypeCoordinates.nullableField("explicitBase"))
//    )

  def updateScienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateScienceTarget",
      fieldType = ListType(TargetEnvironmentEditType[F]),
      arguments = List(ArgumentBulkEditTargetInput),
      resolve   = c => c.target(_.bulkEditScienceTarget(c.arg(ArgumentBulkEditTargetInput)))
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      updateScienceTarget[F]
    )

}

object TargetMutation extends TargetMutation
