// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import lucuma.core.`enum`._
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int._
import lucuma.core.model.SourceProfile
import lucuma.core.optics.syntax.lens._
import lucuma.core.optics.syntax.optional._
import lucuma.gen.gmos.longslit.GmosLongSlit.{AcquisitionSteps, ScienceSteps}
import lucuma.itc.client.{ItcClient, ItcResult}
import lucuma.odb.api.model.GmosModel.{CustomMask, GratingConfig, SouthDynamic, SouthStatic}
import lucuma.odb.api.model.{ObservationModel, ScienceMode, Sequence}
import lucuma.odb.api.model.gmos.syntax.gmosSouthFilter._
import lucuma.odb.api.repo.OdbRepo

import scala.concurrent.duration._

sealed trait GmosSouthLongSlit[F[_]] extends GmosSouthGenerator[F]


/**
 * Sequence generation for GMOS South Longslit
 */
object GmosSouthLongSlit {

  /**
   * Queries the ITC and ODB to come up with a GMOS South LongSlit generator,
   * if possible.
   */
  def query[F[_]: Sync](
    itc:         ItcClient[F],
    odb:         OdbRepo[F],
    observation: ObservationModel,
    sampling:    PosDouble = GmosLongSlit.DefaultSampling,
  ): F[Either[ItcResult.Error, Option[GmosSouthLongSlit[F]]]] =

    GmosLongSlit.Input.query(itc, odb, observation, sampling) {
      case gnls: ScienceMode.GmosSouthLongSlit => gnls
    }.map(_.map(_.map(fromInput[F])))

  def fromInput[F[_]: Sync](
    in: GmosLongSlit.Input[ScienceMode.GmosSouthLongSlit]
  ): GmosSouthLongSlit[F] =
    apply(in.mode, in.λ, in.imageQuality, in.sampling, in.sourceProfile, in.acqTime, in.sciTime, in.exposureCount)

  def apply[F[_]: Sync](
    mode:          ScienceMode.GmosSouthLongSlit,
    λ:             Wavelength,
    imageQuality:  ImageQuality,
    sampling:      PosDouble,
    sourceProfile: SourceProfile,
    acqTime:       AcqExposureTime,
    sciTime:       SciExposureTime,
    exposureCount: PosInt
  ): GmosSouthLongSlit[F] =

    new GmosSouthLongSlit[F] with GmosLongSlit[F, SouthStatic, SouthDynamic] {

      override def static: SouthStatic =
        SouthStatic(
          detector      = GmosSouthDetector.Hamamatsu,
          mosPreImaging = MosPreImaging.IsNotMosPreImaging,
          nodAndShuffle = Option.empty,
          stageMode     = GmosSouthStageMode.FollowXy
        )

      override def acquisitionSteps: AcquisitionSteps[SouthDynamic] =
        Acquisition.compute(mode.fpu, acqTime, λ)

      override def scienceSteps: ScienceSteps[SouthDynamic] =
        Science.compute(mode, sciTime, λ, sourceProfile, imageQuality, sampling)

      override def acquisition(
        recordedSteps: List[RecordedStep[SouthDynamic]]
      ): F[Sequence[SouthDynamic]] =
        longSlitAcquisition(recordedSteps)

      override def science(
        recordedSteps: List[RecordedStep[SouthDynamic]]
      ): F[Sequence[SouthDynamic]] =
        longSlitScience(exposureCount, recordedSteps)
    }

  object Acquisition extends GmosSouthSequenceState {

    def compute(
      fpu:          GmosSouthFpu,
      exposureTime: AcqExposureTime,
      λ:            Wavelength,
    ): AcquisitionSteps[SouthDynamic] = {

      def filter: GmosSouthFilter = GmosSouthFilter.allAcquisition.minBy { f =>
        (λ.toPicometers.value.value - f.wavelength.toPicometers.value.value).abs
      }

      eval {
        for {
          _  <- SouthDynamic.exposure      := exposureTime.value
          _  <- SouthDynamic.filter        := filter.some
          _  <- SouthDynamic.fpu           := none[Either[CustomMask, GmosSouthFpu]]
          _  <- SouthDynamic.gratingConfig := none[GratingConfig[GmosSouthGrating]]
          _  <- SouthDynamic.xBin          := GmosXBinning.Two
          _  <- SouthDynamic.yBin          := GmosYBinning.Two
          _  <- SouthDynamic.roi           := GmosRoi.Ccd2
          s0 <- scienceStep(0.arcsec, 0.arcsec)

          _  <- SouthDynamic.exposure      := 20.seconds
          _  <- SouthDynamic.fpu           := fpu.asRight.some
          _  <- SouthDynamic.xBin          := GmosXBinning.One
          _  <- SouthDynamic.yBin          := GmosYBinning.One
          _  <- SouthDynamic.roi           := GmosRoi.CentralStamp
          s1 <- scienceStep(10.arcsec, 0.arcsec)

          _  <- SouthDynamic.exposure      := exposureTime.value * 4
          s2 <- scienceStep(0.arcsec, 0.arcsec)

        } yield AcquisitionSteps(s0, s1, s2)
      }

    }

  }

  object Science extends GmosSouthSequenceState {

    def compute(
      mode:          ScienceMode.GmosSouthLongSlit,
      exposureTime:  SciExposureTime,
      λ:             Wavelength,
      sourceProfile: SourceProfile,
      imageQuality:  ImageQuality,
      sampling:      PosDouble
    ): ScienceSteps[SouthDynamic] =

      eval {
        for {
          _  <- SouthDynamic.exposure      := exposureTime.value
          _  <- SouthDynamic.xBin          := mode.xBin(sourceProfile, imageQuality, sampling)
          _  <- SouthDynamic.yBin          := mode.yBin
          _  <- SouthDynamic.gratingConfig := GratingConfig(mode.grating, GmosGratingOrder.One, λ).some
          _  <- SouthDynamic.filter        := mode.filter
          _  <- SouthDynamic.fpu           := mode.fpu.asRight.some
          s0 <- scienceStep(0.arcsec, 0.arcsec)
          f0 <- flatStep

          _  <- SouthDynamic.wavelength    := GmosLongSlit.wavelengthDither(λ, mode.λDithers.last)
          s1 <- scienceStep(0.arcsec, 15.arcsec)
          f1 <- flatStep
        } yield ScienceSteps(s0, f0, s1, f1)
      }


  }

}
