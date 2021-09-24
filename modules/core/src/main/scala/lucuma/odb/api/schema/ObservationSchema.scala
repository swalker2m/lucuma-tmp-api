// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ObservationModel
import lucuma.odb.api.repo.{OdbRepo, TableState, Tables}
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.Observation
import cats.MonadError
import cats.data.State
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.odb.api.schema.TargetSchema.TargetEnvironmentModelType
import sangria.schema._

import scala.collection.immutable.Seq


object ObservationSchema {

  import ConstraintSetSchema.ConstraintSetType
  import ScienceConfigurationSchema._
  import ExecutionSchema.ExecutionType
  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, NonEmptyStringType, PlannedTimeSummaryType}
  import ProgramSchema.ProgramType
  import ScienceRequirementsSchema.ScienceRequirementsType

  import context._
  import syntax.`enum`._

  implicit val ObservationIdType: ScalarType[Observation.Id] =
    ObjectIdSchema.idType[Observation.Id](name = "ObservationId")

  implicit val ObsStatusType: EnumType[ObsStatus] =
    EnumType.fromEnumerated(
      "ObsStatus",
      "Observation status options"
    )

  implicit val ObsActiveStatusType: EnumType[ObsActiveStatus] =
    EnumType.fromEnumerated(
      "ObsActiveStatus",
      "Observation operational/active status options"
    )

  val ObservationIdArgument: Argument[Observation.Id] =
    Argument(
      name         = "observationId",
      argumentType = ObservationIdType,
      description  = "Observation ID"
    )

  val OptionalObservationIdArgument: Argument[Option[Observation.Id]] =
    Argument(
      name         = "observationId",
      argumentType = OptionInputType(ObservationIdType),
      description  = "Observation ID"
    )

  val OptionalListObservationIdArgument: Argument[Option[Seq[Observation.Id]]] =
    Argument(
      name         = "observationIds",
      argumentType = OptionInputType(ListInputType(ObservationIdType)),
      description  = "Observation IDs"
    )

  def ObservationType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], ObservationModel] =
    ObjectType(
      name     = "Observation",
      fieldsFn = () => fields(

        Field(
          name        = "id",
          fieldType   = ObservationIdType,
          description = Some("Observation ID"),
          resolve     = _.value.id
        ),

        Field(
          name        = "existence",
          fieldType   = EnumTypeExistence,
          description = Some("Deleted or Present"),
          resolve     = _.value.existence
        ),

        Field(
          name        = "name",
          fieldType   = OptionType(NonEmptyStringType),
          description = Some("Observation name"),
          resolve     = _.value.name
        ),

        Field(
          name        = "status",
          fieldType   = ObsStatusType,
          description = Some("Observation status"),
          resolve     = _.value.status
        ),

        Field(
          name        = "activeStatus",
          fieldType   = ObsActiveStatusType,
          description = "Observation operational status".some,
          resolve     = _.value.activeStatus
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeSummaryType[F],
          description = Some("Observation planned time calculation."),
          resolve     = _.value.plannedTimeSummary
        ),

        Field(
          name        = "program",
          fieldType   = ProgramType[F],
          description = Some("The program that contains this observation"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.program(_.unsafeSelect(c.value.programId, c.includeDeleted))
        ),

        Field(
          name        = "targets",
          fieldType   = OptionType(TargetEnvironmentModelType[F]),
          description = "The observation's target(s)".some,
          resolve     = c => c.target(_.selectTargetEnvironmentForObservation(c.value.id))
        ),

        Field(
          name        = "constraintSet",
          fieldType   = ConstraintSetType[F],
          description = Some("The constraint set for the observation"),
          resolve     = c => c.value.constraintSet
        ),

        Field(
          name        = "scienceRequirements",
          fieldType   = ScienceRequirementsType[F],
          description = Some("The top level science requirements"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.scienceRequirements
        ),

        Field(
          name        = "scienceConfiguration",
          fieldType   = OptionType(ScienceConfigurationType[F]),
          description = Some("The science configuration"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.scienceConfiguration
        ),

        Field(
          name        = "manualConfig",
          fieldType   = OptionType(InstrumentConfigSchema.ConfigType[F]),
          description = Some("Manual instrument configuration"),
          resolve     = c => c.unsafeToFuture {
            c.ctx.tables.get.map { tables =>
              c.value.config.flatMap { icm =>
                icm.dereference[State[Tables, *], Tables](TableState).runA(tables).value
              }
            }
          }
        ),

        Field(
          name        = "execution",
          fieldType   = ExecutionType[F],
          description = Some("Execution sequence and runtime artifacts"),
          resolve     = _.value.id
        )

      )
    )

  def ObservationEdgeType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Edge[ObservationModel]] =
    Paging.EdgeType[F, ObservationModel](
      "ObservationEdge",
      "An observation and its cursor",
      ObservationType[F]
    )

  def ObservationConnectionType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Connection[ObservationModel]] =
    Paging.ConnectionType[F, ObservationModel](
      "ObservationConnection",
      "Matching observations",
      ObservationType[F],
      ObservationEdgeType[F]
    )

}
