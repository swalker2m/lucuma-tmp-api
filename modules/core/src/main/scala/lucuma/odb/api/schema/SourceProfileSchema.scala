// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.data.NonEmptyList
import cats.syntax.option._
import coulomb.Quantity
import coulomb.si.Kelvin
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.`enum`.{Band, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.{BrightnessUnits, BrightnessValue, Wavelength}
import lucuma.core.math.BrightnessUnits.{Brightness, FluxDensityContinuum, Integrated, LineFlux, Surface}
import lucuma.core.math.dimensional.{GroupedUnitQty, GroupedUnitType}
import lucuma.core.model.SpectralDefinition.{BandNormalized, EmissionLines}
import lucuma.core.model.UnnormalizedSED.{BlackBody, CoolStarModel, Galaxy, HIIRegion, Planet, PlanetaryNebula, PowerLaw, Quasar, StellarLibrary, UserDefined}
import lucuma.core.model.{BandBrightness, EmissionLine, UnnormalizedSED}
import lucuma.core.syntax.string._
import sangria.schema.{Field, _}
import sangria.validation.FloatCoercionViolation

import scala.reflect.ClassTag

object SourceProfileSchema {

  import GeneralSchema.PosBigDecimalType
  import syntax.`enum`._
  import WavelengthSchema.WavelengthType

  implicit val EnumTypeBand: EnumType[Band] =
    EnumType.fromEnumerated(
      "Band",
      "Magnitude bands"
    )

  implicit val EnumTypeStellarLibrarySpectrum: EnumType[StellarLibrarySpectrum] =
    EnumType.fromEnumerated(
      "StellarLibrarySpectrum",
      "Stellar library spectrum"
    )

  implicit val EnumTypeGalaxySpectrum: EnumType[GalaxySpectrum] =
    EnumType.fromEnumerated(
      "GalaxySpectrum",
      "Galaxy spectrum"
    )

  implicit val EnumTypePlanetSpectrum: EnumType[PlanetSpectrum] =
    EnumType.fromEnumerated(
      "PlanetSpectrum",
      "Planet spectrum"
    )

  implicit val EnumTypeQuasarSpectrum: EnumType[QuasarSpectrum] =
    EnumType.fromEnumerated(
      "QuasarSpectrum",
      "Quasar spectrum"
    )

  implicit val EnumTypeHiiRegionSpectrum: EnumType[HIIRegionSpectrum] =
    EnumType.fromEnumerated(
      "HiiRegionSpectrum",
      "HII Region spectrum"
    )

  implicit val EnumTypePlanetaryNebulaSpectrum: EnumType[PlanetaryNebulaSpectrum] =
    EnumType.fromEnumerated(
      "PlanetaryNebulaSpectrum",
      "Planetary nebula spectrum"
    )

  def UnnormalizedSedType[C]: OutputType[UnnormalizedSED] =
    UnionType(
      name        = "UnnormalizedSed",
      description = "Un-normalized spectral energy distribution".some,
      types       = List(
        StellarLibraryType[C],
        CoolStarModelType[C],
        GalaxyType[C],
        PlanetType[C],
        QuasarType[C],
        HiiRegionType[C],
        PlanetaryNebulaType[C],
        PowerLawType[C],
        BlackBodyType[C],
        UserDefinedType[C]
      )
    ).mapValue[UnnormalizedSED](identity)

  private def SpectrumEnumBasedSed[C, T: ClassTag, E](
    name:        String,
    enumType:    EnumType[E],
    extractEnum: T => E
  ): ObjectType[C, T] = {
    println(name)
    println(enumType.name)

    ObjectType(
      name        = name,
      fieldsFn    = () => fields(

        Field(
          name        = "spectrum",
          fieldType   = enumType,
          resolve     = c => extractEnum(c.value)
        )
      )
    )
  }

  private def KelvinBasedSed[C, T: ClassTag](
    name:          String,
    description:   String,
    extractKelvin: T => Quantity[PosBigDecimal, Kelvin]
  ): ObjectType[C, T] =
    ObjectType(
      name        = name,
      description = description,
      fieldsFn    = () => fields(
        Field(
          name        = "temperature",
          description = "Kelvin".some,
          fieldType   = PosBigDecimalType,
          resolve     = c => extractKelvin(c.value).value
        )
      )
    )


  def StellarLibraryType[C]: ObjectType[C, StellarLibrary]  =
    SpectrumEnumBasedSed[C, StellarLibrary, StellarLibrarySpectrum](
      "StellarLibrary",
      EnumTypeStellarLibrarySpectrum,
      _.librarySpectrum
    )

  def CoolStarModelType[C]: ObjectType[C, CoolStarModel] =
    KelvinBasedSed[C, CoolStarModel](
      name        = "CoolStarModel",
      description = "Cool star model SED",
      _.temperature
    )

  def GalaxyType[C]: ObjectType[C, Galaxy]  =
    SpectrumEnumBasedSed[C, Galaxy, GalaxySpectrum](
      "Galaxy",
      EnumTypeGalaxySpectrum,
      _.galaxySpectrum
    )

  def PlanetType[C]: ObjectType[C, Planet]  =
    SpectrumEnumBasedSed[C, Planet, PlanetSpectrum](
      "Planet",
      EnumTypePlanetSpectrum,
      _.planetSpectrum
    )

  def QuasarType[C]: ObjectType[C, Quasar]  =
    SpectrumEnumBasedSed[C, Quasar, QuasarSpectrum](
      "Quasar",
      EnumTypeQuasarSpectrum,
      _.quasarSpectrum
    )

  def HiiRegionType[C]: ObjectType[C, HIIRegion]  =
    SpectrumEnumBasedSed[C, HIIRegion, HIIRegionSpectrum](
      "HiiRegion",
      EnumTypeHiiRegionSpectrum,
      _.hiiRegionSpectrum
    )

  def PlanetaryNebulaType[C]: ObjectType[C, PlanetaryNebula] =
    SpectrumEnumBasedSed[C, PlanetaryNebula, PlanetaryNebulaSpectrum](
      "PlanetaryNebula",
      EnumTypePlanetaryNebulaSpectrum,
      _.planetaryNebulaSpectrum
    )

  def PowerLawType[C]: ObjectType[C, PowerLaw] =
    ObjectType(
      name        = "PowerLaw",
      description = "Power law SED",
      fieldsFn    = () => fields(
        Field(
          name        = "index",
          fieldType   = BigDecimalType,
          resolve     = _.value.index
        )
      )
    )

  def BlackBodyType[C]: ObjectType[C, BlackBody] =
    KelvinBasedSed[C, BlackBody](
      name        = "BlackBody",
      description = "Black body SED",
      _.temperature
    )

  def FluxDensityEntryType[C]: ObjectType[C, (Wavelength, PosBigDecimal)] =
    ObjectType(
      name        = "FluxDensityEntry",
      fieldsFn    = () => fields(

        Field(
          name      = "wavelength",
          fieldType = WavelengthType[C],
          resolve   = _.value._1
        ),

        Field(
          name      = "value",
          fieldType = PosBigDecimalType,
          resolve   = _.value._2
        )
      )
    )

  def UserDefinedType[C]: ObjectType[C, UserDefined] =
    ObjectType(
      name        = "UserDefined",
      description = "User defined SED",
      fieldsFn    = () => fields(
        Field(
          name        = "fluxDensities",
          fieldType   = ListType(FluxDensityEntryType[C]),
          resolve     = _.value.fluxDensities.toNel.toList
        )
      )
    )

  // We are limited in the characters we can use for enum names.  These mappings
  // define how to map illegal characters into something valid for GraphQL.
  private val replacements: List[(Char, Char)] =
    List(
      ' ' -> '_',
      '²' -> '2'
    )

  private def defineUnitsEnum[UG](
    name:        String,
    description: String,
    values:      NonEmptyList[GroupedUnitType[UG]]
  ): EnumType[GroupedUnitType[UG]] =
    EnumType(
      name        = name,
      description = description.some,
      values      = values.toList.map { gut =>
        val ud = gut.ungrouped.definition
        println(ud.name)
        EnumValue(
          name        = replacements
                          .foldLeft(ud.name) { case (n, (a, b)) => n.replace(a, b) }
                          .toScreamingSnakeCase,
          description = ud.abbv.some,
          value       = gut
        )
      }
    )

  val EnumTypeBrightnessIntegrated: EnumType[GroupedUnitType[Brightness[Integrated]]] =
    defineUnitsEnum(
      "BrightnessIntegrated",
      "Brightness integrated units",
      BrightnessUnits.Brightness.Integrated.all
    )

  val EnumTypeBrightnessSurface: EnumType[GroupedUnitType[Brightness[Surface]]] =
    defineUnitsEnum(
      "BrightnessSurface",
      "Brightness surface units",
      BrightnessUnits.Brightness.Surface.all
    )

  val EnumTypeLineFluxIntegrated: EnumType[GroupedUnitType[LineFlux[Integrated]]] =
    defineUnitsEnum(
      "LineFluxIntegrated",
      "Line flux integrated units",
      BrightnessUnits.LineFlux.Integrated.all
    )

  val EnumTypeLineFluxSurface: EnumType[GroupedUnitType[LineFlux[Surface]]] =
    defineUnitsEnum(
      "LineFluxSurface",
      "Line flux surface units",
      BrightnessUnits.LineFlux.Surface.all
    )

  val EnumTypeFluxDensityContinuumIntegrated: EnumType[GroupedUnitType[FluxDensityContinuum[Integrated]]] =
    defineUnitsEnum(
      "FluxDensityContinuumIntegrated",
      "Flux density continuum integrated units",
      BrightnessUnits.FluxDensityContinuum.Integrated.all
    )

  val EnumTypeFluxDensityContinuumSurface: EnumType[GroupedUnitType[FluxDensityContinuum[Surface]]] =
    defineUnitsEnum(
      "FluxDensityContinuumSurface",
      "Flux density continuum surface units",
      BrightnessUnits.FluxDensityContinuum.Surface.all
    )

  val BrightnessValueType: ScalarAlias[BrightnessValue, BigDecimal] =
    ScalarAlias(
      BigDecimalType,
      BrightnessValue.fromBigDecimal.reverseGet,
      bd => BrightnessValue.fromBigDecimal.getOption(bd).toRight(FloatCoercionViolation)
    )

  private def GroupedUnitQtyType[C, N, UG](
    name:      String,
    valueType: OutputType[N],
    unitsType: EnumType[GroupedUnitType[UG]]
  ): ObjectType[C, GroupedUnitQty[N, UG]] =
    ObjectType(
      name      = name,
      fieldsFn  = () => fields(

        Field(
          name      = "value",
          fieldType = valueType,
          resolve   = _.value.value
        ),

        Field(
          name      = "units",
          fieldType = unitsType,
          resolve   = _.value.unit
        )
      )
    )

  private def BandBrightnessType[C, T](
    unitCategoryName: String,
    unitsType:        EnumType[GroupedUnitType[Brightness[T]]]
  ): ObjectType[C, BandBrightness[T]] =
    ObjectType(
      name     = s"BandBrightness$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name        = "magnitude",
          fieldType   = GroupedUnitQtyType[C, BrightnessValue, Brightness[T]](
                   s"Magnitude$unitCategoryName",
                          BrightnessValueType,
                          unitsType
                        ),
          resolve     = _.value.quantity
        ),

        Field(
          name        = "band",
          fieldType   = EnumTypeBand,
          description = "Magnitude band".some,
          resolve     = _.value.band
        ),

        Field(
          name        = "error",
          fieldType   = OptionType(BigDecimalType),
          description = "Error, if any".some,
          resolve     = _.value.error.map(BrightnessValue.fromBigDecimal.reverseGet)
        )

      )
    )

  def BandBrightnessIntegrated[C]: ObjectType[C, BandBrightness[Integrated]] =
    BandBrightnessType[C, Integrated](
      "Integrated",
      EnumTypeBrightnessIntegrated
    )

  def BandBrightnessSurface[C]: ObjectType[C, BandBrightness[Surface]] =
    BandBrightnessType[C, Surface](
      "Surface",
      EnumTypeBrightnessSurface
    )

  private def BandNormalizedType[C, T](
    unitCategoryName:   String,
    bandBrightnessType: ObjectType[C, BandBrightness[T]]
  ): ObjectType[C, BandNormalized[T]] =
    ObjectType(
      name     = s"BandNormalized$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name        = "sed",
          fieldType   = UnnormalizedSedType[C],
          description = "Un-normalized spectral energy distribution".some,
          resolve     = _.value.sed
        ),

        Field(
          name        = "brightnesses",
          fieldType   = ListType(bandBrightnessType),
          resolve     = _.value.brightnesses.toList.map(_._2)
        )
      )
    )

  def BandNormalizedIntegrated[C]: ObjectType[C, BandNormalized[Integrated]] =
    BandNormalizedType[C, Integrated](
      "Integrated",
      BandBrightnessIntegrated[C]
    )

  def BandNormalizedSurface[C]: ObjectType[C, BandNormalized[Surface]] =
    BandNormalizedType[C, Surface](
      "Surface",
      BandBrightnessSurface[C]
    )

  private def EmissionLineType[C, T](
    unitCategoryName: String,
    unitsType: EnumType[GroupedUnitType[LineFlux[T]]]
  ): ObjectType[C, EmissionLine[T]] =
    ObjectType(
      name     = s"EmissionLine$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name        = "wavelength",
          fieldType   = WavelengthType[C],
          resolve     = _.value.wavelength
        ),

        // TODO: units like -> "kilometersPerSecond": 1  ?
        Field(
          name        = "lineWidth",
          fieldType   = PosBigDecimalType,
          description = "km/s".some,
          resolve     = _.value.lineWidth.value
        ),

        Field(
          name        = "lineFlux",
          fieldType   = GroupedUnitQtyType[C, PosBigDecimal, LineFlux[T]](
                   s"LineFlux$unitCategoryName",
                          PosBigDecimalType,
                          unitsType
                        ),
          resolve     = _.value.lineFlux
        )
      )
    )

  def EmissionLineIntegrated[C]: ObjectType[C, EmissionLine[Integrated]] =
    EmissionLineType[C, Integrated](
      "Integrated",
      EnumTypeLineFluxIntegrated
    )

  def EmissionLineSurface[C]: ObjectType[C, EmissionLine[Surface]] =
    EmissionLineType[C, Surface](
      "Surface",
      EnumTypeLineFluxSurface
    )

  private def EmissionLinesType[C, T](
    unitCategoryName: String,
    lineType:         ObjectType[C, EmissionLine[T]],
    fdcUnitsType:     EnumType[GroupedUnitType[FluxDensityContinuum[T]]]
  ): ObjectType[C, EmissionLines[T]] =
    ObjectType(
      name     = s"EmissionLines$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name      = "lines",
          fieldType = ListType(lineType),
          resolve   = _.value.lines.values.toList
        ),

        Field(
          name      = "fluxDensityContinuum",
          fieldType = GroupedUnitQtyType[C, PosBigDecimal, FluxDensityContinuum[T]](
                        s"FluxDensityContinuum$unitCategoryName",
                        PosBigDecimalType,
                        fdcUnitsType
                      ),
          resolve   = _.value.fluxDensityContinuum
        )
      )
    )

  def EmissionLinesIntegrated[C]: ObjectType[C, EmissionLines[Integrated]] =
    EmissionLinesType[C, Integrated](
      "Integrated",
      EmissionLineIntegrated[C],
      EnumTypeFluxDensityContinuumIntegrated
    )

  def EmissionLinesSurface[C]: ObjectType[C, EmissionLines[Surface]] =
    EmissionLinesType[C, Surface](
      "Surface",
      EmissionLineSurface[C],
      EnumTypeFluxDensityContinuumSurface
    )

}
