// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{Observation, Program}
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}

import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.all.NonEmptyString
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbObservationModel {

  import ArbConstraintSetModel._
  import ArbScienceRequirements._
  import ArbEnumerated._
  import ArbGid._
  import ArbTargetEnvironmentModel._

  def arbObservationModelWithPid(pid: Program.Id): Arbitrary[ObservationModel] =
    Arbitrary {
      for {
        id <- arbitrary[Observation.Id]
        ex <- arbitrary[Existence]
        nm <- arbitrary[Option[NonEmptyString]]
        os <- arbitrary[ObsStatus]
        as <- arbitrary[ObsActiveStatus]
        ts <- arbitrary[TargetEnvironmentModel]
        cs <- arbitrary[ConstraintSetModel]
        sr <- arbitrary[ScienceRequirements]
      } yield ObservationModel(id, ex, pid, nm, os, as, ts, cs, sr, None, None, PlannedTimeSummaryModel.Zero)
    }

  implicit val arbObservationModel: Arbitrary[ObservationModel] =
    Arbitrary {
      for {
        p <- arbitrary[Program.Id]
        o <- arbObservationModelWithPid(p).arbitrary
      } yield o
    }

  implicit val cogObservationModel: Cogen[ObservationModel] =
    Cogen[(
      Observation.Id,
      Existence,
      Program.Id,
      Option[String],
      ObsStatus,
      ObsActiveStatus,
      TargetEnvironmentModel,
      ConstraintSetModel,
      ScienceRequirements
    )].contramap { in => (
      in.id,
      in.existence,
      in.programId,
      in.name.map(_.value),
      in.status,
      in.activeStatus,
      in.targets,
      in.constraintSet,
      in.scienceRequirements
    )}

  implicit val arbObservationModelCreate: Arbitrary[ObservationModel.Create] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Observation.Id]]
        pd <- arbitrary[Program.Id]
        nm <- arbitrary[Option[NonEmptyString]]
        st <- arbitrary[Option[ObsStatus]]
        as <- arbitrary[Option[ObsActiveStatus]]
        ts <- arbitrary[Option[TargetEnvironmentModel.CreateTargetEnvironmentInput]]
        cs <- arbitrary[Option[ConstraintSetModel.Create]]
      } yield ObservationModel.Create(
        id,
        pd,
        nm,
        st,
        as,
        ts,
        cs,
        None,
        None,
        None
      )
    }

  implicit val cogObservationModelCreate: Cogen[ObservationModel.Create] =
    Cogen[(
      Option[Observation.Id],
      Program.Id,
      Option[String],
      Option[ObsStatus],
      Option[ObsActiveStatus],
      Option[TargetEnvironmentModel.CreateTargetEnvironmentInput],
      Option[ConstraintSetModel.Create]
    )].contramap { in => (
      in.observationId,
      in.programId,
      in.name.map(_.value),
      in.status,
      in.activeStatus,
      in.targets,
      in.constraintSet
    )}

}

object ArbObservationModel extends ArbObservationModel
