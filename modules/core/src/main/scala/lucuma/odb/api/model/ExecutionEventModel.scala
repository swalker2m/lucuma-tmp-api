// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.mtl.Stateful
import lucuma.core.model.{ExecutionEvent, Observation, Step}
import lucuma.core.util.Enumerated
import cats.{Eq, Monad, Order}
import cats.syntax.all._
import eu.timepit.refined.types.numeric._
import io.chrisdavenport.cats.time.instances.instant._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import eu.timepit.refined.auto._

import java.time.Instant

/**
 * Shared interface for all execution events: sequence, step and dataset.
 */
sealed trait ExecutionEventModel {

  def id:            ExecutionEvent.Id

  def observationId: Observation.Id

  def generated:     Instant

  def received:      Instant

}

object ExecutionEventModel {

  implicit val EqExecutionEventModel: Eq[ExecutionEventModel] =
    Eq.instance {
      case (e0: SequenceEvent, e1: SequenceEvent) => e0 === e1
      case (e0: StepEvent,     e1: StepEvent)     => e0 === e1
      case (e0: DatasetEvent,  e1: DatasetEvent)  => e0 === e1
      case _                                      => false
    }

  // Sequence-level Events ----------------------------------------------------

  sealed abstract class SequenceCommandType(
    val tag:       String,
    val shortName: String
  ) extends Product with Serializable


  object SequenceCommandType {

    case object Abort    extends SequenceCommandType("ABORT", "Abort")
    case object Continue extends SequenceCommandType("CONTINUE", "Continue")
    case object Pause    extends SequenceCommandType("PAUSE", "Pause")
    case object Slew     extends SequenceCommandType("SLEW", "Slew")
    case object Start    extends SequenceCommandType("START", "Start")
    case object Stop     extends SequenceCommandType("STOP", "Stop")

    val all: List[SequenceCommandType] =
      List(
        Abort,
        Continue,
        Pause,
        Slew,
        Start,
        Stop
      )

    def fromTag(s: String): Option[SequenceCommandType] =
      all.find(_.tag === s)

    def unsafeFromTag(s: String): SequenceCommandType =
      fromTag(s).getOrElse(throw new NoSuchElementException(s"SequenceCommandType: Invalid tag: '$s'"))

    implicit val EnumeratedSequenceCommandType: Enumerated[SequenceCommandType] =
      new Enumerated[SequenceCommandType] {
        def all: List[SequenceCommandType] = SequenceCommandType.all
        def tag(a: SequenceCommandType): String = a.tag
        override def unsafeFromTag(s: String): SequenceCommandType =
          SequenceCommandType.unsafeFromTag(s)
      }

  }

  final case class SequenceEvent(
    id:            ExecutionEvent.Id,
    observationId: Observation.Id,
    generated:     Instant,
    received:      Instant,
    command:       SequenceCommandType
  ) extends ExecutionEventModel

  object SequenceEvent {

    implicit val OrderSequenceEvent: Order[SequenceEvent] = {
      Order.by { a => (
        a.id,
        a.observationId,
        a.command,
        a.generated,
        a.received
      )}
    }

    final case class Add(
      eventId:       Option[ExecutionEvent.Id],
      observationId: Observation.Id,
      generated:     Instant,
      command:       SequenceCommandType
    ) {

      def add[F[_]: Monad, T](
        db:       DatabaseState[T],
        received: Instant
      )(implicit S: Stateful[F, T]): F[ValidatedInput[SequenceEvent]] =
        for {
          i <- db.executionEvent.getUnusedId(eventId)
          o <- db.observation.lookupValidated[F](observationId)
          e  = (i, o).mapN((iʹ, _) => SequenceEvent(iʹ, observationId, generated, received, command))
          _ <- db.executionEvent.saveNewIfValid(e)(_.id)
        } yield e

    }

    object Add {

      implicit val DecoderAdd: Decoder[Add] =
        deriveDecoder[Add]

      implicit val OrderAdd: Order[Add] =
        Order.by { a => (
          a.eventId,
          a.observationId,
          a.command,
          a.generated
        )}

    }

  }


  // Step-level Events --------------------------------------------------------

  sealed abstract class StepStageType(
    val tag:       String,
    val shortName: String
  ) extends Product with Serializable

  object StepStageType {
    case object EndConfigure   extends StepStageType("END_CONFIGURE", "EndConfigure")
    case object EndObserve     extends StepStageType("END_OBSERVE", "EndObserve")
    case object EndStep        extends StepStageType("END_STEP", "EndStep")
    case object StartConfigure extends StepStageType("START_CONFIGURE", "StartConfigure")
    case object StartObserve   extends StepStageType("START_OBSERVE", "StartObserve")
    case object StartStep      extends StepStageType("START_STEP", "StartStep")

    val all: List[StepStageType] =
      List(
        EndConfigure,
        EndObserve,
        EndStep,
        StartConfigure,
        StartObserve,
        StartStep
      )

    def fromTag(s: String): Option[StepStageType] =
      all.find(_.tag === s)

    def unsafeFromTag(s: String): StepStageType =
      fromTag(s).getOrElse(throw new NoSuchElementException(s"StepStageType: Invalid tag: '$s'"))

    implicit val EnumeratedStepStageType: Enumerated[StepStageType] =
      new Enumerated[StepStageType] {
        override def all: List[StepStageType] = StepStageType.all
        override def tag(a: StepStageType): String = a.tag
        override def unsafeFromTag(s: String): StepStageType = StepStageType.unsafeFromTag(s)
      }

  }

  final case class StepEvent(
    id:            ExecutionEvent.Id,
    observationId: Observation.Id,
    generated:     Instant,
    received:      Instant,

    stepId:        Step.Id,
    sequenceType:  SequenceModel.SequenceType,

    stage:         StepStageType

  ) extends ExecutionEventModel

  object StepEvent {

    implicit val OrderStepEvent: Order[StepEvent] =
      Order.by { a => (
        a.id,
        a.observationId,
        a.stepId,
        a.sequenceType,

        a.stage,

        a.received,
        a.generated
      )}


    final case class Add(
      eventId:       Option[ExecutionEvent.Id],
      observationId: Observation.Id,
      generated:     Instant,

      stepId:        Step.Id,
      sequenceType:  SequenceModel.SequenceType,
      stage:         StepStageType
    ) {

      def add[F[_]: Monad, T](
        db:           DatabaseState[T],
        received:     Instant
      )(implicit S: Stateful[F, T]): F[ValidatedInput[StepEvent]] =

        for {
          i <- db.executionEvent.getUnusedId(eventId)
          o <- db.observation.lookupValidated(observationId)
          s <- db.step.lookupValidated(stepId)
          e  = (i, o, s).mapN { (iʹ, _, _) =>

            StepEvent(
              iʹ,
              observationId,
              generated,
              received,
              stepId,
              sequenceType,
              stage
            )
          }
          _ <- db.executionEvent.saveNewIfValid(e)(_.id)
        } yield e

    }

    object Add {

      implicit val DecoderAdd: Decoder[Add] =
        deriveDecoder[Add]

      implicit val OrderAdd: Order[Add] =
        Order.by { a => (
          a.eventId,
          a.observationId,
          a.stepId,
          a.sequenceType,
          a.stage,
          a.generated
        )}

    }

  }

  // Dataset-level Events -----------------------------------------------------

  sealed abstract class DatasetStageType(
    val tag:       String,
    val shortName: String
  ) extends Product with Serializable

  object DatasetStageType {
    case object EndObserve    extends DatasetStageType("END_OBSERVE", "EndObserve")
    case object EndReadout    extends DatasetStageType("END_READOUT", "EndReadout")
    case object EndWrite      extends DatasetStageType("END_WRITE", "EndWrite")
    case object StartObserve  extends DatasetStageType("START_OBSERVE", "StartObserve")
    case object StartReadout  extends DatasetStageType("START_READOUT", "StartReadout")
    case object StartWrite    extends DatasetStageType("START_WRITE", "StartWrite")

    val all: List[DatasetStageType] =
      List(
        EndObserve,
        EndReadout,
        EndWrite,
        StartObserve,
        StartReadout,
        StartWrite
      )

    def fromTag(s: String): Option[DatasetStageType] =
      all.find(_.tag === s)

    def unsafeFromTag(s: String): DatasetStageType =
      fromTag(s).getOrElse(throw new NoSuchElementException(s"DatasetStageType: Invalid tag: '$s'"))

    implicit val EnumeratedDatasetStageType: Enumerated[DatasetStageType] =
      new Enumerated[DatasetStageType] {
        override def all: List[DatasetStageType] = DatasetStageType.all
        override def tag(a: DatasetStageType): String = a.tag
        override def unsafeFromTag(s: String): DatasetStageType = DatasetStageType.unsafeFromTag(s)
      }

  }

  final case class DatasetEvent(
    id:            ExecutionEvent.Id,
    observationId: Observation.Id,
    generated:     Instant,
    received:      Instant,

    stepId:        Step.Id,
    datasetIndex:  PosInt,
    filename:      Option[DatasetFilename],
    stageType:     DatasetStageType
  ) extends ExecutionEventModel {

    def toDataset: Option[DatasetModel] =
      filename.map { fn =>
        DatasetModel(
          stepId,
          datasetIndex,
          observationId,
          fn
        )
      }

  }

  object DatasetEvent {

    implicit val OrderDatasetEvent: Order[DatasetEvent] =
      Order.by { a => (
        a.id,
        a.observationId,
        a.stepId,
        a.datasetIndex.value,
        a.filename,
        a.stageType,
        a.generated,
        a.received
      )}

    final case class Add(
      eventId:       Option[ExecutionEvent.Id],
      observationId: Observation.Id,
      generated:     Instant,

      stepId:        Step.Id,
      datasetIndex:  PosInt,
      filename:      Option[DatasetFilename],
      stageType:     DatasetStageType
    ) {

      def add[F[_]: Monad, T](
        db:           DatabaseState[T],
        received:     Instant
      )(implicit S: Stateful[F, T]): F[ValidatedInput[DatasetEvent]] =

        for {
          i <- db.executionEvent.getUnusedId(eventId)
          o <- db.observation.lookupValidated(observationId)
          s <- db.step.lookupValidated(stepId)
          e  = (i, o, s).mapN { (iʹ, _, _) =>

            DatasetEvent(
              iʹ,
              observationId,
              generated,
              received,
              stepId,
              datasetIndex,
              filename,
              stageType
            )

          }
          _ <- db.executionEvent.saveNewIfValid(e)(_.id)
        } yield e

    }

    object Add {

      implicit val DecoderAdd: Decoder[Add] =
        deriveDecoder[Add]

      implicit val OrderAdd: Order[Add] =
        Order.by { a => (
          a.eventId,
          a.observationId,
          a.generated,
          a.stepId,
          a.datasetIndex.value,
          a.filename,
          a.stageType
        )}
    }

  }

}
