// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{ConstraintSet, Observation, Program}
import lucuma.core.model.arb.ArbConstraintSet
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.all.NonEmptyString
import lucuma.odb.api.model.targetModel.{TargetEnvironmentInput, TargetEnvironmentModel}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbObservationModel {

  import ArbInput._
  import ArbConstraintSet._
  import ArbConstraintSetInput._
  import ArbScienceRequirements._
  import ArbEnumerated._
  import ArbGid._
  import ArbTargetModel._

  def arbObservationModelWithPid(pid: Program.Id): Arbitrary[ObservationModel] =
    Arbitrary {
      for {
        id <- arbitrary[Observation.Id]
        ex <- arbitrary[Existence]
        nm <- arbitrary[Option[NonEmptyString]]
        os <- arbitrary[ObsStatus]
        as <- arbitrary[ObsActiveStatus]
        ts <- arbitrary[TargetEnvironmentModel]
        cs <- arbitrary[ConstraintSet]
        sr <- arbitrary[ScienceRequirements]
      } yield ObservationModel(id, ex, pid, ObservationModel.Properties(nm, os, as, ts, cs, sr, None, None))
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
      ConstraintSet,
      ScienceRequirements
    )].contramap { in => (
      in.id,
      in.existence,
      in.programId,
      in.properties.subtitle.map(_.value),
      in.properties.status,
      in.properties.activeStatus,
      in.properties.constraintSet,
      in.properties.scienceRequirements
    )}

  implicit val arbObservationModelPropertiesInput: Arbitrary[ObservationModel.PropertiesInput] =
    Arbitrary {
      for {
        nm <- arbitrary[Input[NonEmptyString]]
        st <- arbitrary[Input[ObsStatus]]
        as <- arbitrary[Input[ObsActiveStatus]]
        ts <- arbitrary[Input[TargetEnvironmentInput]]
        cs <- arbitrary[Input[ConstraintSetInput]]
      } yield ObservationModel.PropertiesInput(
        nm,
        st,
        as,
        ts,
        cs,
        Input.ignore,
        Input.ignore,
        Input.ignore
      )
    }

  implicit val cogObservationModelPropertiesInput: Cogen[ObservationModel.PropertiesInput] =
    Cogen[(
      Input[String],
      Input[ObsStatus],
      Input[ObsActiveStatus],
      Input[TargetEnvironmentInput],
      Input[ConstraintSetInput]
    )].contramap { in => (
      in.subtitle.map(_.value),
      in.status,
      in.activeStatus,
      in.targetEnvironment,
      in.constraintSet
    )}

  implicit val arbObservationModelPatchInput: Arbitrary[ObservationModel.PatchInput] =
    Arbitrary {
      for {
        p <- arbitrary[Input[ObservationModel.PropertiesInput]]
        e <- arbitrary[Input[Existence]]
      } yield ObservationModel.PatchInput(p, e)
    }

  implicit val cogObservationModelPatchInput: Cogen[ObservationModel.PatchInput] =
    Cogen[(
      Input[ObservationModel.PropertiesInput],
      Input[Existence]
    )].contramap { in => (
      in.properties,
      in.existence
    )}

  implicit val arbObservationModelCreate: Arbitrary[ObservationModel.CreateInput] =
    Arbitrary {
      for {
        pd <- arbitrary[Program.Id]
        pr <- arbitrary[Option[ObservationModel.PropertiesInput]]
      } yield ObservationModel.CreateInput(pd, pr)
    }

  implicit val cogObservationModelCreate: Cogen[ObservationModel.CreateInput] =
    Cogen[(
      Program.Id,
      Option[ObservationModel.PropertiesInput]
    )].contramap { in => (
      in.programId,
      in.properties
    )}

  implicit val arbObservationModelCloneInput: Arbitrary[ObservationModel.CloneInput] =
    Arbitrary {
      for {
        ex <- arbitrary[Observation.Id]
        pi <- arbitrary[Option[ObservationModel.PatchInput]]
      } yield ObservationModel.CloneInput(ex, pi)
    }

  implicit val cogObservationModelCloneInput: Cogen[ObservationModel.CloneInput] =
    Cogen[(
      Observation.Id,
      Option[ObservationModel.PatchInput]
    )].contramap { in => (
      in.observationId,
      in.patch
    )}
}

object ArbObservationModel extends ArbObservationModel
