// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Atom, ExecutionEvent, Observation, Program, Step, Target}
import lucuma.odb.api.model.{AtomModel, ExecutionEventModel, ObservationModel, ProgramModel, StepModel, TargetEnvironment, TargetEnvironmentModel, TargetModel}
import cats.instances.order._
import monocle.Lens
import monocle.function.At

import scala.collection.immutable.{SortedMap, TreeMap}

/**
 * Simplistic immutable database "tables" of top-level types keyed by Id.
 */
final case class Tables(
  ids:                Ids,
  atoms:              SortedMap[Atom.Id, AtomModel[Step.Id]],
  executionEvents:    SortedMap[ExecutionEvent.Id, ExecutionEventModel],
  observations:       SortedMap[Observation.Id, ObservationModel],
  programs:           SortedMap[Program.Id, ProgramModel],
  steps:              SortedMap[Step.Id, StepModel[_]],
  targets:            SortedMap[Target.Id, TargetModel],
  targetEnvironments: SortedMap[TargetEnvironment.Id, TargetEnvironmentModel]
)

object Tables extends TableOptics {

  val empty: Tables =
    Tables(
      ids                = Ids.zero,
      atoms              = TreeMap.empty[Atom.Id, AtomModel[Step.Id]],
      executionEvents    = TreeMap.empty[ExecutionEvent.Id, ExecutionEventModel],
      observations       = TreeMap.empty[Observation.Id, ObservationModel],
      programs           = TreeMap.empty[Program.Id, ProgramModel],
      steps              = TreeMap.empty[Step.Id, StepModel[_]],
      targets            = TreeMap.empty[Target.Id, TargetModel],
      targetEnvironments = TreeMap.empty[TargetEnvironment.Id, TargetEnvironmentModel]
    )

}

sealed trait TableOptics { self: Tables.type =>

  val ids: Lens[Tables, Ids] =
    Lens[Tables, Ids](_.ids)(b => a => a.copy(ids = b))

  val lastEventId: Lens[Tables, Long] =
    ids.andThen(Ids.lastEvent)

  val lastAtomId: Lens[Tables, Atom.Id] =
    ids.andThen(Ids.lastAtom)

  val lastExecutionEventId: Lens[Tables, ExecutionEvent.Id] =
    ids.andThen(Ids.lastExecutionEvent)

  val lastObservationId: Lens[Tables, Observation.Id] =
    ids.andThen(Ids.lastObservation)

  val lastProgramId: Lens[Tables, Program.Id] =
    ids.andThen(Ids.lastProgram)

  val lastStepId: Lens[Tables, Step.Id] =
    ids.andThen(Ids.lastStep)

  val lastTargetId: Lens[Tables, Target.Id] =
    ids.andThen(Ids.lastTarget)

  val lastTargetEnvironmentId: Lens[Tables, TargetEnvironment.Id] =
    ids.andThen(Ids.lastTargetEnvironment)

  val atoms: Lens[Tables, SortedMap[Atom.Id, AtomModel[Step.Id]]] =
    Lens[Tables, SortedMap[Atom.Id, AtomModel[Step.Id]]](_.atoms)(b => a => a.copy(atoms = b))

  def atom(aid: Atom.Id): Lens[Tables, Option[AtomModel[Step.Id]]] =
    atoms.andThen(At.at(aid))

  val executionEvents: Lens[Tables, SortedMap[ExecutionEvent.Id, ExecutionEventModel]] =
    Lens[Tables, SortedMap[ExecutionEvent.Id, ExecutionEventModel]](_.executionEvents)(b => a => a.copy(executionEvents = b))

  def executionEvent(eid: ExecutionEvent.Id): Lens[Tables, Option[ExecutionEventModel]] =
    executionEvents.andThen(At.at(eid))

  val observations: Lens[Tables, SortedMap[Observation.Id, ObservationModel]] =
    Lens[Tables, SortedMap[Observation.Id, ObservationModel]](_.observations)(b => a => a.copy(observations = b))

  def observation(oid: Observation.Id): Lens[Tables, Option[ObservationModel]] =
    observations.andThen(At.at(oid))

  val programs: Lens[Tables, SortedMap[Program.Id, ProgramModel]] =
    Lens[Tables, SortedMap[Program.Id, ProgramModel]](_.programs)(b => a => a.copy(programs = b))

  def program(pid: Program.Id): Lens[Tables, Option[ProgramModel]] =
    programs.andThen(At.at(pid))

  val steps: Lens[Tables, SortedMap[Step.Id, StepModel[_]]] =
    Lens[Tables, SortedMap[Step.Id, StepModel[_]]](_.steps)(b => a => a.copy(steps = b))

  def step(sid: Step.Id): Lens[Tables, Option[StepModel[_]]] =
    steps.andThen(At.at(sid))

  val targets: Lens[Tables, SortedMap[Target.Id, TargetModel]] =
    Lens[Tables, SortedMap[Target.Id, TargetModel]](_.targets)(b => _.copy(targets = b))

  def target(tid: Target.Id): Lens[Tables, Option[TargetModel]] =
    targets.andThen(At.at(tid))

  val targetEnvironments: Lens[Tables, SortedMap[TargetEnvironment.Id, TargetEnvironmentModel]] =
    Lens[Tables, SortedMap[TargetEnvironment.Id, TargetEnvironmentModel]](_.targetEnvironments)(b => _.copy(targetEnvironments = b))

  def targetEnvironment(vid: TargetEnvironment.Id): Lens[Tables, Option[TargetEnvironmentModel]] =
    targetEnvironments.andThen(At.at(vid))

}
