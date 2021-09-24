// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.model.{TargetEnvironmentModel, TargetModel}
import cats.{Monad, MonadError}
import cats.data.Nested
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.Target
import sangria.schema._


trait TargetQuery {
  import context._

  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{ OptionalObservationIdArgument, ObservationIdType }
  import ProgramSchema.ProgramIdArgument
  import TargetSchema.{OptionalTargetEnvironmentIdArgument, TargetEnvironmentIdType, TargetEnvironmentModelType, TargetModelType, TargetType}

  private def lookupScienceTargets[F[_]: Monad](
    c: Context[OdbRepo[F], Unit]
  ): F[List[TargetModel]] =
    for {
      e <- c.arg(OptionalTargetEnvironmentIdArgument).traverse(c.ctx.target.selectScienceTargetList)
      o <- c.arg(OptionalObservationIdArgument).traverse(c.ctx.target.selectScienceTargetListForObservation)
    } yield e.orElse(o).toList.flatten

  def scienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "scienceTarget",
      fieldType   = OptionType(TargetModelType[F]),
      description = "The first (or only) science target (if any) for the given observation (or environment)".some,
      arguments   = List(OptionalObservationIdArgument, OptionalTargetEnvironmentIdArgument),
      resolve     = c => c.unsafeToFuture(lookupScienceTargets[F](c).map(_.headOption))
    )

  def scienceTargetList[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "scienceTargetList",
      fieldType   = ListType(TargetModelType[F]),
      description = "All science targets (if any) for the given observation (or environment)".some,
      arguments   = List(OptionalObservationIdArgument, OptionalTargetEnvironmentIdArgument),
      resolve     = c => c.unsafeToFuture(lookupScienceTargets[F](c))
    )

  def targetEnvironment[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "targetEnvironment",
      fieldType   = OptionType(TargetEnvironmentModelType[F]),
      description = "Target environment for the given observation (or environment id)".some,
      arguments   = List(OptionalObservationIdArgument, OptionalTargetEnvironmentIdArgument),
      resolve     = c => c.unsafeToFuture {
        for {
          e <- c.arg(OptionalTargetEnvironmentIdArgument).flatTraverse(c.ctx.target.selectTargetEnvironment)
          o <- c.arg(OptionalObservationIdArgument).flatTraverse(c.ctx.target.selectTargetEnvironmentForObservation)
        } yield e.orElse(o)
      }
    )

  def TargetEnvironmentGroupType[F[_]: Dispatcher, A](
    valueName: String,
    outType:   OutputType[A]
  )(implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], TargetEnvironmentModel.Group[A]] =
    ObjectType(
      name     = "TargetEnvironmentGroup",
      fieldsFn = () => fields(

        Field(
          name        = "targetEnvironmentIds",
          fieldType   = ListType(TargetEnvironmentIdType),
          description = "IDs of target environments that share the common value".some,
          resolve     = _.value.targetEnvironmentIds.toList
        ),

        Field(
          name        = "observationIds",
          fieldType   = ListType(ObservationIdType),
          description = "IDs of observations that share the common value".some,
          resolve     = c => c.target { repo =>
            Nested(c.value.targetEnvironmentIds.toList.traverse(repo.unsafeSelectTargetEnvironment))
              .map(_.observationId.toList)
              .value
              .map(_.flatten)
          }
        ),

        Field(
          name        = "targetEnvironments",
          fieldType   = ListType(TargetEnvironmentModelType[F]),
          description = "Target environments that share the common value".some,
          resolve     = c => c.target { repo =>
            c.value.targetEnvironmentIds.toList.traverse(repo.unsafeSelectTargetEnvironment)
          }
        ),

        Field(
          name        = valueName,
          fieldType   = outType,
          description = "Commonly held value across the target environments".some,
          resolve     = _.value.value
        )
      )
    )

  def groupByScienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "ScienceTargetGroup",
      fieldType   = ListType(TargetEnvironmentGroupType[F, Target]("target", TargetType[F])),
      description = "Target environments grouped by those that share the same target".some,
      arguments   = List(
        ProgramIdArgument,
        ArgumentIncludeDeleted
      ),
      resolve     = c => c.target(_.groupBySingleScienceTarget(c.programId, c.includeDeleted))
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      scienceTarget[F],
      scienceTargetList[F],
      targetEnvironment[F],
      groupByScienceTarget[F]
    )
}

object TargetQuery extends TargetQuery
