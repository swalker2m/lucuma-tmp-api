// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.core.model.User
import lucuma.odb.api.repo.OdbRepo
import lucuma.sso.client.SsoClient

import cats.effect.{Async, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.Stream
import org.http4s.implicits._
import org.http4s.HttpApp
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._
import org.typelevel.log4cats.{Logger => Log4CatsLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.ExecutionContext.global
import lucuma.graphql.routes.SangriaGraphQLService
import lucuma.graphql.routes.GraphQLService
import lucuma.odb.api.schema.OdbSchema
import cats.effect.std.Dispatcher
import lucuma.graphql.routes.Routes
import org.http4s.HttpRoutes

// #server
object Main extends IOApp {

  def httpApp[F[_]: Log4CatsLogger: Async](
    userClient: SsoClient[F, User],
    odb:        OdbRepo[F],
  ): Resource[F, HttpApp[F]] =
    Dispatcher[F].map { implicit d =>
      Logger.httpApp(logHeaders = true, logBody = false) {

          // Routes for static resources, ie. GraphQL Playground
          val staticRoutes: HttpRoutes[F] =
            resourceServiceBuilder[F]("/assets").toRoutes

          // Our GraphQL service
          val graphQLService: GraphQLService[F] =
            new SangriaGraphQLService(OdbSchema[F], odb, OdbSchema.exceptionHandler)

          // Our GraphQL routes
          val graphQLRoutes: HttpRoutes[F] =
            Routes.forService[F](graphQLService, userClient, "odb", "ws")

          // Done!
          (staticRoutes <+> graphQLRoutes).orNotFound

        }

    }


  def stream[F[_]: Log4CatsLogger: Async](
    odb: OdbRepo[F],
    cfg: Config
  ): Stream[F, Nothing] = {

    // Spin up the server ...
    for {
      sso       <- Stream.resource(cfg.ssoClient[F])
      userClient = sso.map(_.user)
      httpApp   <- Stream.resource(httpApp(userClient, odb))
      exitCode  <- BlazeServerBuilder[F](global)
        .bindHttp(cfg.port, "0.0.0.0")
        .withHttpApp(httpApp)
        .withWebSockets(true)
        .serve
    } yield exitCode
  }.drain

  def run(args: List[String]): IO[ExitCode] =
    for {
      cfg  <- Config.fromCiris.load(Async[IO])
      log  <- Slf4jLogger.create[IO]
      odb  <- OdbRepo.create[IO]
      _    <- Init.initialize(odb)
      _    <- stream(odb, cfg)(log, Async[IO]).compile.drain
    } yield ExitCode.Success
}

