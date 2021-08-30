// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo

import cats.MonadError
import cats.effect.std.Dispatcher
import sangria.schema._

/**
 * Mutations offered by the API.
 */
object MutationType {

  def apply[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Unit] =
    ObjectType(
      name   = "Mutation",
      fields =
        ProgramMutation.allFields[F]       ++
        ObservationMutation.allFields[F]   ++
        TargetMutation.allFields[F]        ++
        ExecutionEventMutation.allFields[F]

    )

}
