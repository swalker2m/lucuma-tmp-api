// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.{Nested, NonEmptyList, NonEmptyMap}
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.validated._
import coulomb.Quantity
import coulomb.si.Kelvin
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosBigDecimal
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import lucuma.core.`enum`.{Band, CoolStarTemperature, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.BrightnessUnits.Brightness
import lucuma.core.math.dimensional.{Measure, Of, Units}
import lucuma.core.math.{BrightnessValue, Wavelength}
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.{BandBrightness, UnnormalizedSED}
import lucuma.odb.api.model.{InputError, ValidatedInput, WavelengthModel}

import scala.collection.immutable.SortedMap

/**
 * SourceProfile GraphQL schema support model.
 */
object SourceProfileModel {

  final case class FluxDensityEntry(
    wavelength: Wavelength,
    density:    PosBigDecimal
  )

  object FluxDensityEntry {

    def EqFluxDensityEntry: Eq[FluxDensityEntry] =
      Eq.by { a => (
        a.wavelength,
        a.density
      )}

  }

  final case class FluxDensityInput(
    wavelength: WavelengthModel.Input,
    density:    PosBigDecimal
  ) {

    val toFluxDensityEntry: ValidatedInput[FluxDensityEntry] =
      wavelength.toWavelength("wavelength").map { w =>
        FluxDensityEntry(w, density)
      }

  }

  object FluxDensityInput {

    implicit val DecoderFluxDensityInput: Decoder[FluxDensityInput] =
      deriveDecoder[FluxDensityInput]

    implicit val EqFluxDensityInput: Eq[FluxDensityInput] =
      Eq.by { a => (
        a.wavelength,
        a.density
      )}

  }

  final case class UnnormalizedSedInput(
    stellarLibrary:  Option[StellarLibrarySpectrum],
    coolStar:        Option[CoolStarTemperature],
    galaxy:          Option[GalaxySpectrum],
    planet:          Option[PlanetSpectrum],
    quasar:          Option[QuasarSpectrum],
    hiiRegion:       Option[HIIRegionSpectrum],
    planetaryNebula: Option[PlanetaryNebulaSpectrum],
    powerLaw:        Option[BigDecimal],
    blackBodyTempK:  Option[PosBigDecimal],
    fluxDensities:   Option[List[FluxDensityInput]]
  ) {

    val toUserDefined: Option[ValidatedInput[UnnormalizedSED.UserDefined]] =
      fluxDensities.map { fluxDensityInputs =>
        NonEmptyList
          .fromList(fluxDensityInputs)
          .toValidNec(InputError.fromMessage("One or more flux densities must be provided for a user defined SED"))
          .andThen(_.traverse(_.toFluxDensityEntry))
          .map { nel =>
            UnnormalizedSED.UserDefined(
              NonEmptyMap(
                nel.head.wavelength -> nel.head.density,
                SortedMap.from {
                  nel.tail.map { entry => entry.wavelength -> entry.density }
                }
              )
            )
          }
      }


    val toUnnormalizedSed: ValidatedInput[UnnormalizedSED] =
      ValidatedInput.requireOne(
        "sed",
        Nested(List[Option[UnnormalizedSED]](
          stellarLibrary .map(UnnormalizedSED.StellarLibrary(_)),
          coolStar       .map(UnnormalizedSED.CoolStarModel(_)),
          galaxy         .map(UnnormalizedSED.Galaxy(_)),
          planet         .map(UnnormalizedSED.Planet(_)),
          quasar         .map(UnnormalizedSED.Quasar(_)),
          hiiRegion      .map(UnnormalizedSED.HIIRegion(_)),
          planetaryNebula.map(UnnormalizedSED.PlanetaryNebula(_)),
          powerLaw       .map(UnnormalizedSED.PowerLaw(_)),
          blackBodyTempK .map(k => UnnormalizedSED.BlackBody(Quantity[PosBigDecimal, Kelvin](k))),
        )).map(_.validNec[InputError]).value :+ toUserDefined
      )
  }

  object UnnormalizedSedInput {

    implicit val DecoderUnnormalizedSedInput: Decoder[UnnormalizedSedInput] =
      deriveDecoder[UnnormalizedSedInput]

    implicit val EqUnnormalizedSedInput: Eq[UnnormalizedSedInput] =
      Eq.by { a => (
        a.stellarLibrary,
        a.coolStar,
        a.galaxy,
        a.planet,
        a.quasar,
        a.hiiRegion,
        a.planetaryNebula,
        a.powerLaw,
        a.blackBodyTempK,
        a.fluxDensities
      )}
  }

  final case class CreateBrightnessInput[T](
    value: BigDecimal,
    units: Units Of Brightness[T]
  ) {

    val toMeasure: Measure[BrightnessValue] Of Brightness[T] =
      units.withValueTagged(BrightnessValue.fromBigDecimal.get(value))

  }

  object CreateBrightnessInput {

    implicit def DecoderCreateBrightnessInput[T](
      implicit ev: Decoder[Units Of Brightness[T]]
    ): Decoder[CreateBrightnessInput[T]] =
      deriveDecoder[CreateBrightnessInput[T]]

    implicit def EqCreateBrightnessInput[T]: Eq[CreateBrightnessInput[T]] =
      Eq.by { a => (
        a.value,
        a.units
      )}

  }

  final case class CreateBandBrightnessInput[T](
    brightness: CreateBrightnessInput[T],
    band:       Band,
    error:      Option[BigDecimal]
  ) {

    val toBandBrightness: BandBrightness[T] =
      BandBrightness(
        brightness.toMeasure,
        band,
        error.map(e => BrightnessValue.fromBigDecimal.get(e))
      )

  }

  object CreateBandBrightnessInput {

    implicit def DecoderCreateBandBrightnessInput[T](
      implicit ev: Decoder[Units Of Brightness[T]]
    ): Decoder[CreateBandBrightnessInput[T]] =
      deriveDecoder[CreateBandBrightnessInput[T]]

    implicit def EqCreateBandBrightnessInput[T]: Eq[CreateBandBrightnessInput[T]] =
      Eq.by { a => (a.brightness, a.band, a.error) }

  }

  final case class CreateBandNormalizedInput[T](
    sed:          UnnormalizedSedInput,
    brightnesses: List[CreateBandBrightnessInput[T]]
  ) {

    def toBandNormalized: ValidatedInput[BandNormalized[T]] =
      sed.toUnnormalizedSed.map { sed =>
        BandNormalized(
          sed,
          SortedMap.from(brightnesses.map(_.toBandBrightness).fproductLeft(_.band))
        )
      }

  }

  object CreateBandNormalizedInput {

    implicit def DecoderCreateBandNormalizedInput[T](
      implicit ev: Decoder[Units Of Brightness[T]]
    ): Decoder[CreateBandNormalizedInput[T]] =
      deriveDecoder[CreateBandNormalizedInput[T]]

    implicit def EqCreateBandNormalizedInput[T]: Eq[CreateBandNormalizedInput[T]] =
      Eq.by { a => (a.sed, a.brightnesses) }

  }

}
