// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import TargetEnvironmentModel.{CreateTargetEnvironmentInput, EditTargetEnvironmentInput}
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget

import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import clue.data.Input
import org.scalacheck._
import eu.timepit.refined.cats._
import org.scalacheck.Arbitrary.arbitrary
import scala.collection.immutable.SortedMap


trait ArbTargetEnvironmentModel {

  import ArbCoordinates._
  import ArbCoordinatesModel._
  import ArbInput._
  import ArbTarget._
  import ArbTargetModel._

  implicit val arbTargetEnvironmentModel: Arbitrary[TargetEnvironmentModel] =
    Arbitrary {
      for {
        b <- arbitrary[Option[Coordinates]]
        s <- arbitrary[List[Target]].map(l => SortedMap.from(l.fproductLeft(_.name)))
      } yield TargetEnvironmentModel(b, s)
    }

  implicit val cogTargetEnvironmentModel: Cogen[TargetEnvironmentModel] =
    Cogen[(
      Option[Coordinates],
      List[Target]
    )].contramap { in => (
      in.explicitBase,
      in.science.values.toList
    )}

  implicit val arbCreateTargetEnvironment: Arbitrary[CreateTargetEnvironmentInput] =
    Arbitrary {
      for {
        b <- arbitrary[Option[CoordinatesModel.Input]]
        s <- arbitrary[List[TargetModel.Create]]
      } yield Create(b, s)
    }

  implicit val cogCreateTargetEnvironment: Cogen[CreateTargetEnvironmentInput] =
    Cogen[(
      Option[CoordinatesModel.Input],
      List[TargetModel.Create]
    )].contramap { in => (
      in.explicitBase,
      in.science
    )}

  implicit val arbEditTargetEnvironment: Arbitrary[EditTargetEnvironmentInput] =
    Arbitrary {
      for {
        b <- arbitrary[Input[CoordinatesModel.Input]]
        e <- arbitrary[Option[TargetModel.EditTargetList]]
      } yield EditTargetEnvironmentInput(b, e)
    }

  implicit val cogEditTargetEnvironment: Cogen[EditTargetEnvironmentInput] =
    Cogen[(
      Input[CoordinatesModel.Input],
      Option[TargetModel.EditTargetList]
    )].contramap { in => (
      in.explicitBase,
      in.science
    )}

}

object ArbTargetEnvironmentModel extends ArbTargetEnvironmentModel
