// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.eq._
import lucuma.core.`enum`.{GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu, GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu, Instrument}
import monocle.macros.GenPrism
import monocle.Prism

sealed trait ScienceMode extends Product with Serializable {

  def instrument: Instrument

}

object ScienceMode {

  /**
   * GmosNorthLongSlit mode. BasicConfig options can be overridden or expanded
   * upon in AdvancedConfig if desired.  The AdvancedConfig serves as the input
   * to sequence generation.
   */
  final case class GmosNorthLongSlit(
    basic:    gmos.longslit.BasicConfig[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu],
    advanced: gmos.longslit.AdvancedConfig[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu]
  ) extends ScienceMode {

    override def instrument: Instrument =
      Instrument.GmosNorth

  }

  object GmosNorthLongSlit {

    implicit val EqGmosNorthLongSlit: Eq[GmosNorthLongSlit] =
      Eq.by { a => (
        a.basic,
        a.advanced
      )}

  }

  final case class GmosSouthLongSlit(
    basic:    gmos.longslit.BasicConfig[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu],
    advanced: gmos.longslit.AdvancedConfig[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu]
  ) extends ScienceMode {

    override def instrument: Instrument =
      Instrument.GmosSouth

  }

  object GmosSouthLongSlit {

    implicit val EqGmosSouthLongSlit: Eq[GmosSouthLongSlit] =
      Eq.by { a => (
        a.basic,
        a.advanced
      )}

  }

  val EqScienceMode: Eq[ScienceMode] =
    Eq.instance {
      case (a @ GmosNorthLongSlit(_, _), b @ GmosNorthLongSlit(_, _)) => a === b
      case (a @ GmosSouthLongSlit(_, _), b @ GmosSouthLongSlit(_, _)) => a === b
      case _                                                          => false
    }

  val gmosNorthLongSlit: Prism[ScienceMode, GmosNorthLongSlit] =
    GenPrism[ScienceMode, GmosNorthLongSlit]

  val gmosSouthLongSlit: Prism[ScienceMode, GmosSouthLongSlit] =
    GenPrism[ScienceMode, GmosSouthLongSlit]

}
