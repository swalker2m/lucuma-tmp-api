// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import cats.effect._
import cats.implicits._
import clue.ApolloWebSocketClient
import clue.ResponseException
import clue.GraphQLOperation
import clue.PersistentStreamingClient
import clue.TransactionalClient
import io.circe.Json
import clue.http4sjdk.Http4sJDKBackend
import clue.http4sjdk.Http4sJDKWSBackend
import io.circe.literal._
import lucuma.core.model.User
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.service.{ Init, Main }
import lucuma.sso.client.SsoClient
import munit.CatsEffectSuite
import org.http4s.{Uri => Http4sUri, _}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.headers.Authorization
import org.http4s.server.Server
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import sttp.model.Uri

/**
 * Mixin that allows execution of GraphQL operations on a per-suite instance of the Odb, shared
 * among all tests.
 */
trait OdbSuite extends CatsEffectSuite {

  private implicit val log: Logger[IO] =
    Slf4jLogger.getLoggerFromClass(getClass)

  private val ssoClient: SsoClient[IO, User] =
    new SsoClient[IO, User] {
      def find(req: Request[IO]): IO[Option[User]] = IO.pure(None)
      def get(authorization: Authorization): IO[Option[User]] = IO.pure(None)
      def require(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] = ???
      def map[B](f: User => B): SsoClient[IO,B] = ???
      def filter(f: User => Boolean): SsoClient[IO,User] = ???
      def collect[B](f: PartialFunction[User,B]): SsoClient[IO,B] = ???
    }

  private val httpApp: Resource[IO, HttpApp[IO]] =
    Resource.eval(OdbRepo.create[IO].flatTap(Init.initialize(_)))
      .flatMap(Main.httpApp(ssoClient, _))

  private val server: Resource[IO, Server] =
    Resource.make(IO.println("  • Server starting..."))(_ => IO.println("  • Server stopped.")) *>
    httpApp.flatMap { app =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .withHttpApp(app)
        .bindAny()
        .resource
        .flatTap(_ => Resource.eval(IO.println("  • Server started.")))
    }

  private def transactionalClient(svr: Server): Resource[IO, TransactionalClient[IO, Nothing]] =
    for {
      xbe <- Http4sJDKBackend.apply[IO]
      uri  = Uri.parse((svr.baseUri / "odb").renderString).toOption.get
      xc  <- Resource.eval(TransactionalClient.of[IO, Nothing](uri)(Async[IO], xbe, Logger[IO]))
    } yield xc

  private def streamingClient(svr: Server): Resource[IO, PersistentStreamingClient[IO, Nothing, _, _]] =
    for {
      sbe <- Http4sJDKWSBackend[IO]
      uri  = Uri.parse((svr.baseUri / "ws").copy(scheme = Some(Http4sUri.Scheme.unsafeFromString("ws"))).renderString).toOption.get
      sc  <- Resource.eval(ApolloWebSocketClient.of(uri)(Async[IO], Logger[IO], sbe))
      _   <- Resource.make(sc.connect() *> sc.initialize())(_ => sc.terminate() *> sc.disconnect())
    } yield sc

  case class Operation(
    document: String
  ) extends GraphQLOperation[Nothing] {
    type Data       = Json
    type Variables  = Json
    val varEncoder  = implicitly
    val dataDecoder = implicitly
  }

  private val serverFixture: Fixture[Server] =
    ResourceSuiteLocalFixture("server", server)

  override def munitFixtures = List(serverFixture)

  sealed trait ClientOption extends Product with Serializable {

    def prefix: String =
      this match {
        case ClientOption.Http => "[http]"
        case ClientOption.Ws   => "[ws]  "
      }

    def connection: Server => Resource[IO, TransactionalClient[IO, Nothing]] =
      this match {
        case ClientOption.Http => transactionalClient
        case ClientOption.Ws   => streamingClient
      }

  }

  object ClientOption {
    case object Http extends ClientOption
    case object Ws   extends ClientOption

    val All: List[ClientOption] = List(Http, Ws)
  }

  /** Run a query and ensure that the responses are as expected. */
  def queryTest(
    query:     String,
    expected:  Json,
    variables: Option[Json] = None,
    clients:   List[ClientOption] = ClientOption.All
  ): Unit =
    queryTestImpl(query, expected.asRight, variables, clients)

  def queryTestFailure(
    query:     String,
    errors:    List[String],
    variables: Option[Json] = None,
    clients:   List[ClientOption] = ClientOption.All
  ): Unit =
    queryTestImpl(query, errors.asLeft, variables, clients)

  private def queryTestImpl(
    query:     String,
    expected:  Either[List[String], Json],
    variables: Option[Json],
    clients:   List[ClientOption]
  ): Unit = {

    def go(client: ClientOption): Unit = {
      val suffix = query.linesIterator.dropWhile(_.trim.isEmpty).next().trim + " ..."
      test(s"${client.prefix} $suffix") {
        Resource.eval(IO(serverFixture()))
          .flatMap(client.connection)
          .use { conn =>
            val req = conn.request(Operation(query))
            val op  = variables.fold(req.apply)(req.apply)

            expected.fold(errors => {
              op.intercept[ResponseException]
                .map(e => e.asGraphQLErrors.toList.flatten.map(_.message))
                .assertEquals(errors)
            }, success => {
              op.map(_.spaces2)
                .assertEquals(success.spaces2) // by comparing strings we get more useful errors
            })
          }
      }
    }

    clients.foreach(go)
  }

  def subscriptionTest(query: String, mutations: Either[List[(String, Option[Json])], IO[Unit]], expected: List[Json], variables: Option[Json] = None) = {
    val suffix = query.linesIterator.dropWhile(_.trim.isEmpty).next().trim + " ..."
    test(s"[sub]  $suffix") {
      Resource.eval(IO(serverFixture()))
        .flatMap(streamingClient)
        .use { conn =>
          val req = conn.subscribe(Operation(query))
          variables
            .fold(req.apply)(req.apply) // awkward API
            .flatMap { sub =>
              for {
                _   <- log.debug("*** ----- about to start stream fiber")
                fib <- sub.stream.compile.toList.start
                _   <- log.debug("*** ----- pausing a bit")
                _   <- IO.sleep(200.millis)
                _   <- log.debug("*** ----- running mutations")
                _   <- mutations.fold(_.traverse_ { case (query, vars) =>
                         val req = conn.request(Operation(query))
                         vars.fold(req.apply)(req.apply)
                       }, identity)
                _   <- log.debug("*** ----- pausing a bit")
                _   <- IO.sleep(200.millis)
                _   <- log.debug("*** ----- stopping subscription")
                _   <- sub.stop()
                _   <- log.debug("*** ----- joining fiber")
                obt <- fib.joinWithNever
              } yield assertEquals(obt.map(_.spaces2), expected.map(_.spaces2))  // by comparing strings we get more useful errors
            }
        }
    }
  }

}
