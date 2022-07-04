// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated
import lucuma.core.model.NonNegDuration
import lucuma.core.model.implicits._
import lucuma.core.syntax.time._
import cats.{Eq, Semigroup}
import cats.data.NonEmptyList
import cats.kernel.Order
import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all._
import lucuma.core.enums.Instrument
import org.typelevel.cats.time.instances.duration._


final case class PlannedTime(
  setup:       NonNegDuration,
  acquisition: List[PlannedTime.CategorizedTime],
  science:     List[PlannedTime.CategorizedTime]
) {

  def acquisitionSum: PlannedTime.CategorizedTime =
    NonEmptyList(PlannedTime.CategorizedTime.Zero, acquisition).reduce

  def scienceSum: PlannedTime.CategorizedTime =
    NonEmptyList(PlannedTime.CategorizedTime.Zero, science).reduce

  def total: NonNegDuration =
    setup |+| acquisition.foldMap(_.total) |+| science.foldMap(_.total)

}

object PlannedTime {

  val Zero: PlannedTime =
    PlannedTime(
      NonNegDuration.zero,
      Nil,
      Nil
    )

  sealed trait Category extends Product with Serializable

  object Category {

    case object ConfigChange extends Category
    case object Exposure     extends Category
    case object Readout      extends Category
    case object Write        extends Category

    val configChange: Category = ConfigChange
    val exposure: Category     = Exposure
    val readout: Category      = Readout
    val write: Category        = Write

    implicit val EnumeratedCategory: Enumerated[Category] = {
      Enumerated.of(ConfigChange, Exposure, Readout, Write)
    }

  }

  sealed abstract class ConfigChangeType(
    val key:        String,
    val name:       String,
    val instrument: Option[Instrument] = None
  ) extends Product with Serializable

  object ConfigChangeType {

    case object GcalDiffuser             extends ConfigChangeType("GcalDiffuser",             "GCAL Diffuser"              )
    case object GcalFilter               extends ConfigChangeType("GcalFilter",               "GCAL Filter"                )
    case object GcalShutter              extends ConfigChangeType("GcalShutter",              "GCAL Shutter"               )
    case object GmosNorthDisperser       extends ConfigChangeType("GmosNorthDisperser",       "GMOS-N Disperser", Instrument.GmosNorth.some)
    case object GmosNorthFilter          extends ConfigChangeType("GmosNorthFilter",          "GMOS-N Filter",    Instrument.GmosNorth.some)
    case object GmosNorthFocalPlaneUnit  extends ConfigChangeType("GmosNorthFocalPlaneUnit",  "GMOS-N FPU",       Instrument.GmosNorth.some)
    case object GmosSouthDisperser       extends ConfigChangeType("GmosSouthDisperser",       "GMOS-S Disperser", Instrument.GmosSouth.some)
    case object GmosSouthFilter          extends ConfigChangeType("GmosSouthFilter",          "GMOS-S Filter",    Instrument.GmosSouth.some)
    case object GmosSouthFocalPlaneUnit  extends ConfigChangeType("GmosSouthFocalPlaneUnit",  "GMOS-S FPU",       Instrument.GmosSouth.some)
    case object ScienceFold              extends ConfigChangeType("ScienceFold",              "Science Fold"               )
    case object TelescopeOffsetBase      extends ConfigChangeType("TelescopeOffsetBase",      "Telescope Offset Base"      )
    case object TelescopeOffsetPerArcsec extends ConfigChangeType("TelescopeOffsetPerArcsec", "Telescope Offset Per Arcsec")

    implicit val EnumeratedConfigChangeType: Enumerated[ConfigChangeType] =
      Enumerated.of(
        GcalDiffuser,
        GcalFilter,
        GcalShutter,
        GmosNorthDisperser,
        GmosNorthFilter,
        GmosNorthFocalPlaneUnit,
        GmosSouthDisperser,
        GmosSouthFilter,
        GmosSouthFocalPlaneUnit,
        ScienceFold,
        TelescopeOffsetBase,
        TelescopeOffsetPerArcsec
      )
  }

  final case class ConfigChangeEntry(
    changeType: ConfigChangeType,
    time:       NonNegDuration
  )

  object ConfigChangeEntry {

    implicit val OrderConfigChangeEntry: Order[ConfigChangeEntry] =
      Order.from { (a, b) =>
        a.time.value.compare(b.time.value) match {
          case 0 => a.changeType.compare(b.changeType)
          case i => i
        }
      }

    def overallCost(entries: Iterable[ConfigChangeEntry]): NonNegDuration =
      entries.maxOption.map(_.time).getOrElse(NonNegDuration.zero)

  }

  final case class CategorizedTime(
    configChange: NonNegDuration,
    exposure:     NonNegDuration,
    readout:      NonNegDuration,
    write:        NonNegDuration
  ) {

    def total: NonNegDuration =
      configChange |+| exposure |+| readout |+| write

    def +(that: CategorizedTime): CategorizedTime =
      CategorizedTime(
        configChange |+| that.configChange,
        exposure     |+| that.exposure,
        readout      |+| that.readout,
        write        |+| that.write
      )

    def addTime(category: Category, time: NonNegDuration): CategorizedTime =
      category match {
        case Category.ConfigChange => copy(configChange |+| time)
        case Category.Exposure     => copy(exposure     |+| time)
        case Category.Readout      => copy(readout      |+| time)
        case Category.Write        => copy(write        |+| time)
      }

  }

  object CategorizedTime {

    private val zeroDuration: NonNegDuration =
      NonNegDuration.unsafeFrom(0L.seconds)

    // Zero but not a valid Monoid zero because of the time units
    val Zero: CategorizedTime =
      CategorizedTime(zeroDuration, zeroDuration, zeroDuration, zeroDuration)

    implicit val EqCategorizedTime: Eq[CategorizedTime] =
      Eq.by(a => (
        a.configChange.value,
        a.exposure.value,
        a.readout.value,
        a.write.value
      ))

    implicit val SemigroupCategorizedTime: Semigroup[CategorizedTime] =
      Semigroup.instance[CategorizedTime](_ + _)

  }

  // Placeholder estimate.  In reality you cannot estimate a step independently
  // like this because you need to account for changes from the previous step.
  def estimateStep[D](s: StepConfig[D]): CategorizedTime = {
    def forExposure(exposure: NonNegDuration): CategorizedTime =
      CategorizedTime(
        configChange = NonNegDuration.unsafeFrom(7.seconds),
        exposure     = NonNegDuration.unsafeFrom(exposure.value),
        readout      = NonNegDuration.unsafeFrom(71400.milliseconds),
        write        = NonNegDuration.unsafeFrom(10.seconds)
      )

    def forDynamicConfig(d: D): CategorizedTime =
      d match {
        case g: GmosModel.NorthDynamic => forExposure(g.exposure)
        case g: GmosModel.SouthDynamic => forExposure(g.exposure)
        case _                         => CategorizedTime.Zero
      }

    s match {
      case StepConfig.Bias(a)       => forDynamicConfig(a)
      case StepConfig.Dark(a)       => forDynamicConfig(a)
      case StepConfig.Gcal(a, _)    => forDynamicConfig(a)
      case StepConfig.Science(a, _) => forDynamicConfig(a)

    }
  }

  def estimateAtom[D](a: AtomModel[StepModel[D]]): CategorizedTime =
    a.steps.map(s => estimateStep(s.config)).reduce

  def estimateSequence[D](s: Sequence[D]): CategorizedTime =
    NonEmptyList(CategorizedTime.Zero, s.atoms.map(estimateAtom)).reduce

  def estimate(config: ExecutionModel): PlannedTime = {
    val gmosSetup = NonNegDuration.unsafeFrom(18.minutes)

    config match {
      case gn: ExecutionModel.GmosNorth =>
        PlannedTime(
          gmosSetup,
          gn.config.acquisition.atoms.map(estimateAtom),
          gn.config.science.atoms.map(estimateAtom)
        )

      case gs: ExecutionModel.GmosSouth =>
        PlannedTime(
          gmosSetup,
          gs.config.acquisition.atoms.map(estimateAtom),
          gs.config.science.atoms.map(estimateAtom)
        )
    }
  }

}
