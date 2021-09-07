// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.sso.client.SsoClient.UserInfo
import lucuma.sso.client.{SsoClient, SsoJwtReader}
import lucuma.sso.client.util.{GpgPublicKeyReader, JwtDecoder}
import cats.effect.{Async, Concurrent, Resource}
import cats.syntax.all._
import ciris.{ConfigDecoder, ConfigValue, env, prop}
import org.http4s.Uri
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.Client

import java.security.PublicKey

/**
 * Application configuration.
 */
final case class Config(
  port:         Int,
  ssoRoot:      Uri,
  ssoPublicKey: PublicKey,
  serviceJwt:   String
) {

  def jwtReader[F[_]: Concurrent]: SsoJwtReader[F] =
    SsoJwtReader(JwtDecoder.withPublicKey(ssoPublicKey))

  def httpClientResource[F[_]: Async]: Resource[F, Client[F]] =
    BlazeClientBuilder(scala.concurrent.ExecutionContext.Implicits.global).resource

  // SSO Client resource (has to be a resource because it owns an HTTP client).
  def ssoClient[F[_]: Async]: Resource[F, SsoClient[F, UserInfo]] =
    httpClientResource[F].evalMap { httpClient =>
      SsoClient.initial(
        serviceJwt = serviceJwt,
        ssoRoot    = ssoRoot,
        jwtReader  = jwtReader[F],
        httpClient = httpClient,
      )
    }
}

object Config {

  implicit val uri: ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI") { s =>
      Uri.fromString(s).toOption
    }

  implicit val publicKey: ConfigDecoder[String, PublicKey] =
    ConfigDecoder[String].mapOption("Public Key") { s =>
      GpgPublicKeyReader.publicKey(s).toOption
    }

  def envOrProp[F[_]](name: String): ConfigValue[F, String] =
    env(name) or prop(name)

  def fromCiris[F[_]]: ConfigValue[F, Config] = (
    (envOrProp("ODB_PORT") or envOrProp("PORT") or ConfigValue.default("8080")).as[Int],
    envOrProp("ODB_SSO_ROOT").as[Uri],
    envOrProp("ODB_SSO_PUBLIC_KEY").as[PublicKey],
    envOrProp("ODB_SERVICE_JWT")
  ).parMapN(Config.apply)

}