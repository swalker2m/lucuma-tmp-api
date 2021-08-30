// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import lucuma.core.model.{Program, Target}
import lucuma.core.model.arb.ArbTarget
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.string._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.SortedMap


trait ArbProgramModel {

  import ArbEnumerated._
  import ArbGid._
  import ArbTarget._
  import ArbTargetModel._

  implicit val arbProgramModel: Arbitrary[ProgramModel] =
    Arbitrary {
      for {
        id <- arbitrary[Program.Id]
        ex <- arbitrary[Existence]
        nm <- arbitrary[Option[NonEmptyString]]
        ts <- arbitrary[List[Target]].map(l => SortedMap.from(l.fproductLeft(_.name)))
      } yield ProgramModel(id, ex, nm, ts)
    }

  implicit val cogProgramModel: Cogen[ProgramModel] =
    Cogen[(
      Program.Id,
      Existence,
      Option[String],
      List[Target]
    )].contramap { in => (
      in.id,
      in.existence,
      in.name.map(_.value),
      in.targetCatalog.values.toList
    )}

  implicit val arbProgramModelCreate: Arbitrary[ProgramModel.Create] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Program.Id]]
        nm <- arbitrary[Option[NonEmptyString]]
        ts <- arbitrary[Option[List[TargetModel.Create]]]
      } yield ProgramModel.Create(id, nm, ts)
    }

  implicit val cogProgramModelCreate: Cogen[ProgramModel.Create] =
    Cogen[(
      Option[Program.Id],
      Option[String],
      Option[List[TargetModel.Create]]
    )].contramap { in => (
      in.programId,
      in.name.map(_.value),
      in.targetCatalog
    )}
}

object ArbProgramModel extends ArbProgramModel
