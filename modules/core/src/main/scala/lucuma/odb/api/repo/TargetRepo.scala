// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.State
import lucuma.core.model.{Observation, Program, Target}
import lucuma.core.util.Gid
import lucuma.odb.api.model.{TargetEnvironment, TargetEnvironmentModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import lucuma.odb.api.model.ProgramModel.ProgramEvent
import lucuma.odb.api.model.TargetEnvironmentModel.Group
import lucuma.odb.api.model.TargetModel.{BulkEditTargetInput, BulkEditTargetListInput, TargetEnvironmentEdit}
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.syntax.toplevel._

import cats.effect.{Async, Ref}
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.implicits.catsKernelOrderingForOrder

import scala.collection.immutable.SortedSet

sealed trait TargetRepo[F[_]] {

  def selectScienceTarget(
    id: Target.Id
  ): F[Option[TargetModel]]

  def unsafeSelectScienceTarget(
    id: Target.Id
  ): F[TargetModel]

  def selectScienceTargetList(
    id: TargetEnvironment.Id
  ): F[List[TargetModel]]

  def selectScienceTargetListForObservation(
    id: Observation.Id
  ): F[List[TargetModel]]

  def selectTargetEnvironment(
    id: TargetEnvironment.Id
  ): F[Option[TargetEnvironmentModel]]

  def unsafeSelectTargetEnvironment(
    id: TargetEnvironment.Id
  ): F[TargetEnvironmentModel]

  def selectTargetEnvironmentForObservation(
    id: Observation.Id
  ): F[Option[TargetEnvironmentModel]]

  def groupBySingleScienceTarget(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[Target]]]

  def groupByScienceTargetList(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[Set[Target]]]]

  def groupByTargetEnvironment(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[TargetEnvironment]]]

  def bulkEditScienceTarget(
    be: BulkEditTargetInput
  ): F[List[TargetEnvironmentEdit]]

  def bulkEditScienceTargetList(
    be: BulkEditTargetListInput
  ): F[List[TargetEnvironmentEdit]]

}

object TargetRepo {

  def create[F[_]: Async](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  ): TargetRepo[F] =

    new TargetRepo[F] {

      override def selectScienceTarget(
        id: Target.Id
      ): F[Option[TargetModel]] =
        tablesRef.get.map(_.targets.get(id))

      private def unsafeSelect[I: Gid, A](
        id: I
      )(
        f:  I => F[Option[A]]
      ): F[A] =
        f(id).flatMap {
          case None    => ExecutionException.missingReference[F,I,A](id)
          case Some(a) => a.pure[F]
        }

      override def unsafeSelectScienceTarget(
        id: Target.Id
      ): F[TargetModel] =
        unsafeSelect(id)(selectScienceTarget)

      override def selectScienceTargetList(
        id: TargetEnvironment.Id
      ): F[List[TargetModel]] =
        tablesRef.get.map(_.targets.values.filter(_.targetEnvironmentId === id).toList)

      override def selectScienceTargetListForObservation(
        id: Observation.Id
      ): F[List[TargetModel]] =
        for {
          e  <- selectTargetEnvironmentForObservation(id)
          ts <- e.map(_.id).traverse(selectScienceTargetList)
        } yield ts.toList.flatten

      override def selectTargetEnvironment(
        id: TargetEnvironment.Id
      ): F[Option[TargetEnvironmentModel]] =
        tablesRef.get.map(_.targetEnvironments.get(id))

      override def unsafeSelectTargetEnvironment(
        id: TargetEnvironment.Id
      ): F[TargetEnvironmentModel] =
        unsafeSelect(id)(selectTargetEnvironment)

      override def selectTargetEnvironmentForObservation(
        id: Observation.Id
      ): F[Option[TargetEnvironmentModel]] =
        tablesRef.get.map(_.targetEnvironments.values.find(_.observationId.contains(id)))

      private def filteredTargets(
        tables:         Tables,
        pid:            Program.Id,
        includeDeleted: Boolean
      ): List[TargetModel] = {

        def includeEnv: TargetEnvironmentModel => Boolean = tm =>
          (tm.programId === pid) &&
            (includeDeleted || tm.observationId.exists { oid =>
              tables.observations.get(oid).exists(_.isPresent)
            })

        tables
          .targets
          .values
          .filter { tm =>
            tables
              .targetEnvironments
              .get(tm.targetEnvironmentId)
              .exists(includeEnv)
          }
          .toList

      }

      override def groupBySingleScienceTarget(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[Target]]] =

        tablesRef.get.map { t =>
          filteredTargets(t, pid, includeDeleted)
           .groupMap(_.target)(_.targetEnvironmentId)
           .map { case (t, vids) => Group(t, SortedSet.from(vids)) }
           .toList
           .sortBy(_.targetEnvironmentIds.head)
        }

      override def groupByScienceTargetList(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[Set[Target]]]] =

        tablesRef.get.map { t =>
          filteredTargets(t, pid, includeDeleted)
            .groupMap(_.targetEnvironmentId)(_.target)
            .map { case (tid, targets) =>
              (Set.from(targets), tid)
            }
            .groupMap(_._1)(_._2)
            .map { case (ts, vids) => Group(ts, SortedSet.from(vids)) }
            .toList
            .sortBy(_.targetEnvironmentIds.head)
        }

      override def groupByTargetEnvironment(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[TargetEnvironment]]] =

        tablesRef.get.map { t =>
          filteredTargets(t, pid, includeDeleted)
            .groupMap(_.targetEnvironmentId)(_.target)
            .map { case (tid, targets) =>
              (t.targetEnvironments(tid).toTargetEnvironment(targets), tid)
            }
            .groupMap(_._1)(_._2)
            .map { case (ts, vids) => Group(ts, SortedSet.from(vids)) }
            .toList
            .sortBy(_.targetEnvironmentIds.head)
        }

      private def bulkEdit(
        edit: State[Tables, ValidatedInput[List[TargetEnvironmentEdit]]]
      ): F[List[TargetEnvironmentEdit]] = {

        val update = tablesRef.modify { t =>
          val (tʹ, v) = edit.run(t).value
          (v.fold(_  => t, _  => tʹ), v)
        }

        for {
          e <- update.flatMap(_.liftTo[F])
          _ <- e.traverse_(p => eventService.publish(ProgramEvent.updated(p.program)))
          _ <- e.flatMap(_.observation.toList).traverse_(o => eventService.publish(ObservationEvent.updated(o)))
        } yield e

      }

      override def bulkEditScienceTarget(
        be: BulkEditTargetInput
      ): F[List[TargetEnvironmentEdit]] =
        bulkEdit(be.edit[State[Tables, *], Tables](TableState))

      override def bulkEditScienceTargetList(
        be: BulkEditTargetListInput
      ): F[List[TargetEnvironmentEdit]] =
        bulkEdit(be.edit[State[Tables, *], Tables](TableState))

    }

}