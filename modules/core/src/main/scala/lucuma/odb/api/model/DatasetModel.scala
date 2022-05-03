// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.{Order, Show}
import cats.syntax.bitraverse._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.`enum`.DatasetQaState
import lucuma.core.model.Observation
import lucuma.core.optics.Format
import monocle.{Lens, Focus}

import scala.util.matching.Regex

final case class DatasetModel(
  id:            DatasetModel.Id,
  observationId: Observation.Id,
  filename:      DatasetFilename,
  qaState:       Option[DatasetQaState]
)

object DatasetModel extends DatasetModelOptics {

  final case class Id(
    stepId: Step.Id,
    index:  PosInt
  )

  object Id {
    implicit val OrderId: Order[Id] =
      Order.by { a => (a.stepId, a.index) }

    val PosIntPattern: Regex =
      raw"([1-9a-f][0-9a-f]*)".r

    val fromString: Format[String, Id] =
      Format(
        _.split(',').toList match {
          case List(sid, PosIntPattern(idx)) =>
            (Step.Id.parse(sid), PosInt.unapply(java.lang.Integer.parseInt(idx)))
            .bisequence
            .map { case (sid, idx) => Id(sid, idx) }
          case _                             =>
            None
        },
        id => s"${Uid[Step.Id].show(id.stepId)},${id.index}"
      )

    implicit val ShowId: Show[Id] =
      Show.show[Id](fromString.reverseGet)

  }

  implicit val OrderDatasetModel: Order[DatasetModel] =
    Order.by { a => (
      a.id,
      a.observationId,
      a.filename,
      a.qaState
    )}

}

sealed trait DatasetModelOptics { self: DatasetModel.type =>

  val qaState: Lens[DatasetModel, Option[DatasetQaState]] =
    Focus[DatasetModel](_.qaState)

}