// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.odb.api.model._
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`._
import lucuma.core.optics.syntax.all._
import lucuma.core.math.syntax.int._
import cats.data.State
import cats.effect.Sync
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.parser.decode
import lucuma.core.model.Program
import lucuma.odb.api.model
import lucuma.odb.api.model.OffsetModel.ComponentInput

import scala.concurrent.duration._

object Init {

  val targetsJson = List(
"""
{
  "name":  "NGC 5949",
  "ra":    { "hms":  "15:28:00.668" },
  "dec":   { "dms": "64:45:47.4"  },
  "epoch": "J2000.000",
  "properMotion": {
    "ra":  { "milliarcsecondsPerYear": 0.0 },
    "dec": { "milliarcsecondsPerYear": 0.0 }
  },
  "radialVelocity": { "metersPerSecond": 423607 },
  "parallax":       { "milliarcseconds":  0.00 },
  "magnitudes": [
    {
      "band": "B",
      "value": 12.7,
      "system": "VEGA"
    },
    {
      "band": "R",
      "value": 12.252,
      "system": "VEGA"
    },
    {
      "band": "J",
      "value": 10.279,
      "system": "VEGA"
    },
    {
      "band": "H",
      "value": 9.649,
      "system": "VEGA"
    },
    {
      "band": "K",
      "value": 9.425,
      "system": "VEGA"
    }
  ]
}
""",
"""
{
  "name":  "NGC 3269",
  "ra":    { "hms":  "10:29:57.070" },
  "dec":   { "dms": "-35:13:27.8"  },
  "epoch": "J2000.000",
  "properMotion": {
    "ra":  { "milliarcsecondsPerYear": 0.0 },
    "dec": { "milliarcsecondsPerYear": 0.0 }
  },
  "radialVelocity": { "metersPerSecond": 3753885 },
  "parallax":       { "milliarcseconds":  0.00 },
  "magnitudes": [
    {
      "band": "B",
      "value": 13.24,
      "system": "VEGA"
    },
    {
      "band": "V",
      "value": 13.51,
      "system": "VEGA"
    },
    {
      "band": "R",
      "value": 11.73,
      "system": "VEGA"
    },
    {
      "band": "J",
      "value": 9.958,
      "system": "VEGA"
    },
    {
      "band": "H",
      "value": 9.387,
      "system": "VEGA"
    },
    {
      "band": "K",
      "value": 9.055,
      "system": "VEGA"
    }
  ]
}
""",
"""
{
  "name":  "NGC 3312",
  "ra":    { "hms": "10:37:02.549" },
  "dec":   { "dms": "-27:33:54.17"  },
  "epoch": "J2000.000",
  "properMotion": {
    "ra":  { "milliarcsecondsPerYear": 0.0 },
    "dec": { "milliarcsecondsPerYear":  0.0 }
  },
  "radialVelocity": { "metersPerSecond": 2826483 },
  "parallax":       { "milliarcseconds":  0.0 },
  "magnitudes": [
    {
      "band": "B",
      "value": 12.63,
      "system": "VEGA"
    },
    {
      "band": "V",
      "value": 13.96,
      "system": "VEGA"
    },
    {
      "band": "J",
      "value": 9.552,
      "system": "VEGA"
    },
    {
      "band": "H",
      "value": 8.907,
      "system": "VEGA"
    },
    {
      "band": "K",
      "value": 8.665,
      "system": "VEGA"
    }
  ]
}
"""
  )

  val targets: Either[Exception, List[TargetModel.CreateSidereal]] =
    targetsJson.traverse(decode[TargetModel.CreateSidereal])

  import GmosModel.{CreateCcdReadout, CreateSouthDynamic}
  import StepConfig.CreateStepConfig

  import CreateSouthDynamic.{exposure, filter, fpu, grating, readout, roi, step}
  import CreateCcdReadout.{ampRead, xBin, yBin}

  private def edit[A](start: A)(state: State[A, _]): A =
    state.runS(start).value

  val gmosAc: CreateSouthDynamic =
    CreateSouthDynamic(
      FiniteDurationModel.Input(10.seconds),
      CreateCcdReadout(
        GmosXBinning.Two,
        GmosYBinning.Two,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Fast
      ),
      GmosDtax.Zero,
      GmosRoi.Ccd2,
      None,
      Some(GmosSouthFilter.RPrime),
      None
    )

  val ac1: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmosAc, OffsetModel.Input.Zero)

  val ac2: CreateStepConfig[CreateSouthDynamic] =
    edit(ac1) {
      for {
        _ <- step.p                                               := ComponentInput(10.arcsec)
        _ <- step.exposure                                        := FiniteDurationModel.Input(20.seconds)
        _ <- step.instrumentConfig.andThen(readout).andThen(xBin) := GmosXBinning.One
        _ <- step.instrumentConfig.andThen(readout).andThen(yBin) := GmosYBinning.One
        _ <- step.instrumentConfig.andThen(roi)               := GmosRoi.CentralStamp
        _ <- step.instrumentConfig.andThen(fpu)                   := GmosSouthFpu.LongSlit_1_00.asRight.some
      } yield ()
    }

  val ac3: CreateStepConfig[CreateSouthDynamic] =
    (step.exposure := (FiniteDurationModel.Input(30.seconds))).runS(ac2).value

  val acquisitionSequence: SequenceModel.Create[CreateSouthDynamic] =
    SequenceModel.Create(
      List(ac1, ac2, ac3).map(AtomModel.Create.continueTo) ++
        List.fill(10)(AtomModel.Create.stopBefore(ac3))
    )

  val gcal: GcalModel.Create =
    GcalModel.Create(
      GcalContinuum.QuartzHalogen.some,
      List.empty[GcalArc],
      GcalFilter.Gmos,
      GcalDiffuser.Visible,
      GcalShutter.Closed
    )

  val Q15: OffsetModel.Input =
    OffsetModel.Input.fromArcseconds(0.0, 15.0)

  val gmos520: CreateSouthDynamic =
    edit(gmosAc) {
      for {
        _ <- exposure                 := FiniteDurationModel.Input.fromSeconds(950.0)
        _ <- readout.andThen(ampRead) := GmosAmpReadMode.Slow
        _ <- readout.andThen(xBin)    := GmosXBinning.Two
        _ <- readout.andThen(yBin)    := GmosYBinning.Two
        _ <- roi                      := GmosRoi.CentralSpectrum
        _ <- grating                  := GmosModel.CreateGrating[GmosSouthDisperser](GmosSouthDisperser.B600_G5323, GmosDisperserOrder.One, WavelengthModel.Input.fromNanometers(520.0)).some
        _ <- filter                   := Option.empty[GmosSouthFilter]
        _ <- fpu                      := GmosSouthFpu.LongSlit_1_00.asRight.some
      } yield ()
    }

  val gmos525: CreateSouthDynamic =
    edit(gmos520)(
      CreateSouthDynamic.instrument.wavelength := WavelengthModel.Input.fromNanometers(525.0)
    )

  val threeSeconds: FiniteDurationModel.Input =
    FiniteDurationModel.Input.fromSeconds(3.0)

  val flat_520: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.gcal(edit(gmos520)(exposure := threeSeconds), gcal)

  val flat_525: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.gcal(edit(gmos525)(exposure := threeSeconds), gcal)

  val sci0_520: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmos520, OffsetModel.Input.Zero)

  val sci15_520: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmos520, Q15)

  val sci0_525: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmos525, OffsetModel.Input.Zero)

  val sci15_525: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmos525, Q15)

  val scienceSequence: SequenceModel.Create[CreateSouthDynamic] =
    SequenceModel.Create(
      List(
        flat_520,  sci0_520,
        sci15_520, flat_520,
        flat_520,  sci15_520,
        sci0_520,  flat_520,
        flat_520,  sci0_520,
        sci15_520, flat_520,

        flat_525,  sci15_525,
        sci0_525,  flat_525,
        flat_525,  sci0_525,
        sci15_525, flat_525
      ).map(StepModel.Create.continueTo)
       .grouped(2) // pairs flat and science steps
       .toList
       .map(AtomModel.Create(None, _))
    )

  def obs(
    pid:   Program.Id,
    target: Option[TargetModel.CreateSidereal]
  ): ObservationModel.Create =
    ObservationModel.Create(
      observationId        = None,
      programId            = pid,
      name                 = target.map(_.name) orElse NonEmptyString.from("Observation").toOption,
      status               = ObsStatus.New.some,
      activeStatus         = ObsActiveStatus.Active.some,
      targets              = target.fold(none[model.TargetEnvironmentModel.Create]) { sidereal =>
        TargetEnvironmentModel.Create.singleSidereal(sidereal).some
      },
      constraintSet        = None,
      scienceRequirements  = ScienceRequirementsModel.Create.Default.some,
      scienceConfiguration = None,
      config               =
        InstrumentConfigModel.Create.gmosSouth(
          GmosModel.CreateSouthStatic.Default,
          acquisitionSequence,
          scienceSequence
        ).some
    )

  /**
   * Initializes a (presumably) empty ODB with some demo values.
   */
  def initialize[F[_]: Sync](repo: OdbRepo[F]): F[Unit] =
    for {
      p  <- repo.program.insert(
              ProgramModel.Create(
                None,
                NonEmptyString.from("The real dark matter was the friends we made along the way").toOption,
                None
              )
            )
      _  <- repo.program.insert(
              ProgramModel.Create(
                None,
                NonEmptyString.from("An Empty Placeholder Program").toOption,
                None
              )
            )
      cs <- targets.liftTo[F]
      _  <- repo.observation.insert(obs(p.id, cs.headOption))
      _  <- repo.observation.insert(obs(p.id, cs.lastOption))
      _  <- repo.observation.insert(obs(p.id, None))
    } yield ()

}
