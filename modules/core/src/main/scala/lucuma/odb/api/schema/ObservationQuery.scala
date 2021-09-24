// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ConstraintSetModel, InputError, ObservationModel, ScienceRequirements}
import lucuma.odb.api.repo.{OdbRepo, ResultPage}
import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.all._
import sangria.schema._

trait ObservationQuery {

  import ConstraintSetSchema.ConstraintSetType
  import context._
  import GeneralSchema.ArgumentIncludeDeleted
  import Paging._
  import ProgramSchema.OptionalProgramIdArgument
  import ObservationSchema.{ObservationIdArgument, ObservationType, ObservationConnectionType, OptionalListObservationIdArgument}
  import ScienceRequirementsSchema.ScienceRequirementsType

  def observations[F[_]: Dispatcher](implicit E: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "observations",
      fieldType   = ObservationConnectionType[F],
      description = "Returns all observations associated with the given ids or program, or all observations if neither is specified.".some,
      arguments   = List(
        OptionalListObservationIdArgument.copy(description = "(Optional) listing of specific observations to retrieve".some),
        OptionalProgramIdArgument.copy(description = "(Optional) program whose observations are sought".some),
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve     = c =>
        unsafeSelectTopLevelPageFuture(c.pagingObservationId) { gid =>
          (c.arg(OptionalListObservationIdArgument), c.arg(OptionalProgramIdArgument)) match {
            case (Some(_), Some(_)) =>
              E.raiseError[ResultPage[ObservationModel]](
                InputError.fromMessage(
                  s"Specify only one of `${OptionalListObservationIdArgument.name}` or `${OptionalProgramIdArgument.name}`"
                ).toException
              )
            case (Some(oids), None) =>
              c.ctx.observation.selectPageForObservations(oids.toSet, c.pagingFirst, gid, c.includeDeleted)
            case (None, Some(pid))  =>
              c.ctx.observation.selectPageForProgram(pid, c.pagingFirst, gid, c.includeDeleted)
            case (None, None)       =>
              c.ctx.observation.selectPage(c.pagingFirst, gid, c.includeDeleted)
          }
        }
    )

  def forId[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "observation",
      fieldType   = OptionType(ObservationType[F]),
      description = "Returns the observation with the given id, if any.".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.observation(_.select(c.observationId, c.includeDeleted))
    )

  def groupByConstraintSet[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =

    ObservationGroupSchema.groupingField[F, ConstraintSetModel](
      "constraintSet",
      "Observations grouped by commonly held constraints",
      ConstraintSetType[F],
      (repo, pid, includeDeleted) => repo.groupByConstraintSet(pid, includeDeleted)
    )

  def groupByScienceRequirements[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =

    ObservationGroupSchema.groupingField[F, ScienceRequirements](
      "scienceRequirements",
      "Observations grouped by commonly held science requirements",
      ScienceRequirementsType[F],
      (repo, pid, includeDeleted) => repo.groupByScienceRequirements(pid, includeDeleted)
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      observations,
      forId,
      groupByConstraintSet,
      groupByScienceRequirements
    )
}

object ObservationQuery extends ObservationQuery
