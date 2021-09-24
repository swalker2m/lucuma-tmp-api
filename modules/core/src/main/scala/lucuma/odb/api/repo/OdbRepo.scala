// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.implicits._
import cats.effect.{Async, Ref}

/**
 * The main "repository" for the API server.  It is simply a collection of
 * repositories for the top-level types.
 */
trait OdbRepo[F[_]] {

  def tables: Ref[F, Tables]

  def eventService: EventService[F]

  def atom: AtomRepo[F]

  def executionEvent: ExecutionEventRepo[F]

  def observation: ObservationRepo[F]

  def program: ProgramRepo[F]

  def step: StepRepo[F]

  def target: TargetRepo[F]

}

object OdbRepo {

  /**
   * Creates an empty ODB repository backed by a `Ref` containing `Tables`.
   */
  def create[F[_]: Async]: F[OdbRepo[F]] =
    fromTables[F](Tables.empty)

  /**
   * Creates an ODB repository backed by a `Ref` containing the given `Tables`.
   */
  def fromTables[F[_]: Async](t: Tables): F[OdbRepo[F]] =
    for {
      r <- Ref.of[F, Tables](t)
      s <- EventService(r)
    } yield new OdbRepo[F] {

      override def tables: Ref[F, Tables] =
        r

      override def eventService: EventService[F] =
        s

      override def atom: AtomRepo[F] =
        AtomRepo.create(r)

      override def executionEvent: ExecutionEventRepo[F] =
        ExecutionEventRepo.create(r)

      override def observation: ObservationRepo[F] =
        ObservationRepo.create(r, s)

      override def program: ProgramRepo[F] =
        ProgramRepo.create(r, s)

      override def step: StepRepo[F] =
        StepRepo.create[F](r)

      override def target: TargetRepo[F] =
        TargetRepo.create[F](r, s)

    }



}
