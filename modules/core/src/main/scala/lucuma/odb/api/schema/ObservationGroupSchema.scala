// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ObservationModel
import lucuma.odb.api.repo.{ObservationRepo, OdbRepo, ResultPage}
import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.{Observation, Program}
import sangria.schema._


object ObservationGroupSchema {

  import context._
  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{ObservationConnectionType, ObservationIdType}
  import Paging._
  import ProgramSchema.ProgramIdArgument

  def ObservationGroupType[F[_]: Dispatcher, A](
    prefix:      String,
    valueName:   String,
    outType:     OutputType[A]
  )(implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], ObservationModel.Group[A]] =

    ObjectType(
      name     = s"${prefix}Group",
      fieldsFn = () => fields(

        Field(
          name        = "observationIds",
          fieldType   = ListType(ObservationIdType),
          description = Some("IDs of observations that use the same constraints"),
          resolve     = _.value.observationIds.toList
        ),

        Field(
          name        = "observations",
          fieldType   = ObservationConnectionType[F],
          description = Some("Observations that use this constraint set"),
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          resolve     = c =>
            unsafeSelectTopLevelPageFuture(c.pagingObservationId) { gid =>
              c.ctx.observation.selectPageFromIds(c.pagingFirst, gid, c.includeDeleted) { _ =>
                c.value.observationIds
              }
            }
        ),

        Field(
          name        = valueName,
          fieldType   = outType,
          description = "Commonly held value across the observations".some,
          resolve     = _.value.value
        )

      )
    )

  def ObservationGroupEdgeType[F[_], A](
    groupType: ObjectType[OdbRepo[F], ObservationModel.Group[A]]
  ): ObjectType[OdbRepo[F], Paging.Edge[ObservationModel.Group[A]]] =

    Paging.EdgeType[F, ObservationModel.Group[A]](
      s"${groupType.name}Edge",
      "An observation group and its cursor",
      groupType
    )

  def ObservationGroupConnectionType[F[_], A](
    groupType: ObjectType[OdbRepo[F], ObservationModel.Group[A]],
    edgeType:  ObjectType[OdbRepo[F], Paging.Edge[ObservationModel.Group[A]]]
  ): ObjectType[OdbRepo[F], Paging.Connection[ObservationModel.Group[A]]] =

    Paging.ConnectionType[F, ObservationModel.Group[A]](
      s"${groupType.name}Connection",
      "Observations grouped by common properties",
      groupType,
      edgeType
    )

  def groupingField[F[_]: Dispatcher, A](
    name:        String,
    description: String,
    outType:     OutputType[A],
    lookupAll:   (ObservationRepo[F], Program.Id, Boolean) => F[List[ObservationModel.Group[A]]]
  )(
    implicit ev: MonadError[F, Throwable]
  ): Field[OdbRepo[F], Unit] = {

    val groupType =
      ObservationGroupSchema.ObservationGroupType[F, A](
        name.capitalize,
        name,
        outType
      )

    val edgeType =
      ObservationGroupSchema.ObservationGroupEdgeType[F, A](
        groupType
      )

    val connectionType =
      ObservationGroupSchema.ObservationGroupConnectionType[F, A](
        groupType,
        edgeType
      )

    Field(
      name        = s"${name}Group",
      fieldType   = connectionType,
      description = description.some,
      arguments   = List(
        ProgramIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve    = c =>
        Paging.unsafeSelectPageFuture[F, Observation.Id, ObservationModel.Group[A]](
          c.pagingObservationId,
          g   => Cursor.gid[Observation.Id].reverseGet(g.observationIds.head),
          oid => lookupAll(c.ctx.observation, c.programId, c.includeDeleted).map { gs =>
            ResultPage.fromSeq(gs, c.arg(ArgumentPagingFirst), oid, _.observationIds.head)
          }
        )
    )
  }

}
