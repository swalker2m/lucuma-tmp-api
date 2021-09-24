// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Atom, ExecutionEvent, Observation, Program, Step, Target}
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.{AtomModel, DatabaseState, ExecutionEventModel, ObservationModel, ProgramModel, RepoState, SharingState, StepModel, TargetEnvironment, TargetEnvironmentModel, TargetModel}
import cats.data.State
import cats.mtl.Stateful
import monocle.Lens

trait TableState extends DatabaseState[Tables] {

  val nextEventId: State[Tables, Long] =
    Tables.lastEventId.mod(_ + 1L)

  override val atom: RepoState[Tables, Atom.Id, AtomModel[Step.Id]] =
    RepoState.fromLenses(Tables.lastAtomId, Tables.atoms)

  override val executionEvent: RepoState[Tables, ExecutionEvent.Id, ExecutionEventModel] =
    RepoState.fromLenses(Tables.lastExecutionEventId, Tables.executionEvents)

  override val observation: RepoState[Tables, Observation.Id, ObservationModel] =
    RepoState.fromLenses(Tables.lastObservationId, Tables.observations)

  override val program: RepoState[Tables, Program.Id, ProgramModel] =
    RepoState.fromLenses(Tables.lastProgramId, Tables.programs)

  override val step: RepoState[Tables, Step.Id, StepModel[_]] =
    RepoState.fromLenses(Tables.lastStepId, Tables.steps)

  override val target: RepoState[Tables, Target.Id, TargetModel] =
    RepoState.fromLenses(Tables.lastTargetId, Tables.targets)

  override val targetEnvironment: RepoState[Tables, TargetEnvironment.Id, TargetEnvironmentModel] =
    RepoState.fromLenses(Tables.lastTargetEnvironmentId, Tables.targetEnvironments)

}

final class SharingStateManyToMany[A, B](lens: Lens[Tables, ManyToMany[A, B]]) extends SharingState[Tables, A, B] {
  def update[F[_]](f: ManyToMany[A, B] => ManyToMany[A, B])(implicit S: Stateful[F, Tables]): F[Unit] =
    S.modify(lens.modify(f))

  override def link[F[_]](link: (A, B))(implicit S: Stateful[F, Tables]): F[Unit] =
    update(_ + link)

  override def unlink[F[_]](link: (A, B))(implicit S: Stateful[F, Tables]): F[Unit] =
    update(_ - link)

  override def linkAll[F[_]](links: List[(A, B)])(implicit S: Stateful[F, Tables]): F[Unit] =
    update(_ ++ links)

  override def unlinkAll[F[_]](links: List[(A, B)])(implicit S: Stateful[F, Tables]): F[Unit] =
    update(_ -- links)

}

object TableState extends TableState
