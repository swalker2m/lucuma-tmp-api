// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.eq._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.{IntPercent, Partner}
import lucuma.odb.api.model.query.WherePredicate

final case class WhereProposalPartners(
  MATCH:   Option[WhereProposalPartnerEntry],
  EQ:      Option[List[Partner]],
  isJoint: Option[Boolean]
) extends WherePredicate[Map[Partner, IntPercent]] {

  override def matches(a: Map[Partner, IntPercent]): Boolean = {
    val aʹ = a.filter { case (_, p) => p.value > 0 }

    MATCH.forall(_.matches(aʹ))              &&
      EQ.forall(_.toSet == a.keySet)         &&
      isJoint.forall(_ === aʹ.sizeIs.>=(1))
  }
}

object WhereProposalPartners {

  implicit val DecoderWhereProposalPartners: Decoder[WhereProposalPartners] =
    deriveDecoder[WhereProposalPartners]

}
