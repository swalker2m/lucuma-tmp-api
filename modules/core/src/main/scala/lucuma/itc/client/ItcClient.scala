// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.data.OptionT
import cats.syntax.all._
import cats.effect.{Async, Resource}
import clue.TransactionalClient
import clue.http4sjdk.Http4sJDKBackend
import lucuma.core.model.Target
import lucuma.odb.api.model.ObservationModel
import org.http4s.Uri
import org.http4s.implicits.http4sLiteralsSyntax
import org.typelevel.log4cats.Logger

object ItcClient {

  val uri: Uri =
    uri"https://itc-staging.herokuapp.com/itc"

  /*
    val variables: Json =
    json"""
{
  "spec": {
    "wavelength": {
      "nanometers": 500
    },
    "signalToNoise": 500,
    "spatialProfile": {
      "sourceType": "POINT_SOURCE"
    },
    "spectralDistribution": {
      "stellar": "O5V"
    },
    "magnitude": {
      "band": "J",
      "value": 15.6
    },
    "radialVelocity": {
      "metersPerSecond": 10
    },
    "constraints": {
      "imageQuality": "POINT_ONE",
      "cloudExtinction": "POINT_ONE",
	    "skyBackground": "DARKEST",
      "waterVapor": "VERY_DRY",
      "elevationRange": {
        "airmassRange": {
          "min": 1.5,
          "max": 2.0
        }
      }
    },
    "modes": [
      {
        "gmosS": {
          "disperser": "B1200_G5321",
          "fpu": "LONG_SLIT_0_50"
        }
      }
    ]
  }
}
      """
      */

  def resource[F[_]: Async: Logger]: Resource[F, TransactionalClient[F, Unit]] =
    for {
      b <- Http4sJDKBackend[F]
      c <- Resource.eval(TransactionalClient.of[F, Unit](uri)(Async[F], b, Logger[F]))
    } yield c


  def query[F[_]: Async: Logger](
    o: ObservationModel,
    t: Target
  ): F[Option[ItcSpectroscopyResult]] =
    (for {
      inp <- OptionT(Async[F].pure(ItcSpectroscopyInput.fromObservation(o, t)))
      res <- OptionT(resource[F].use(_.request(ItcQuery)(inp)).map(_.headOption))
    } yield res).value

}