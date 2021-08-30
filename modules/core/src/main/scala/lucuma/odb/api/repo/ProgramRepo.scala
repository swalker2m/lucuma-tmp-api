// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.Program
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.{Event, InputError, ProgramModel, ValidatedInput}
import lucuma.odb.api.model.ProgramModel.ProgramEvent
import lucuma.odb.api.model.syntax.validatedinput._
import cats.implicits._
import cats.MonadError
import cats.data.{EitherT, State}
import cats.effect.Ref


trait ProgramRepo[F[_]] extends TopLevelRepo[F, Program.Id, ProgramModel] {

  def selectPageForPrograms(
    pids:           Set[Program.Id],
    count:          Option[Int]        = None,
    afterGid:       Option[Program.Id] = None,
    includeDeleted: Boolean            = false
  ): F[ResultPage[ProgramModel]]

  def insert(input: ProgramModel.Create): F[ProgramModel]

  def edit(input: ProgramModel.Edit): F[ProgramModel]
}

object ProgramRepo {

  def create[F[_]](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): ProgramRepo[F] =

    new TopLevelRepoBase[F, Program.Id, ProgramModel](
      tablesRef,
      eventService,
      Tables.lastProgramId,
      Tables.programs,
      (editType, model) => ProgramEvent(_, editType, model)
    ) with ProgramRepo[F]
      with LookupSupport {

      override def selectPageForPrograms(
        pids:           Set[Program.Id],
        count:          Option[Int]        = None,
        afterGid:       Option[Program.Id] = None,
        includeDeleted: Boolean            = false
      ): F[ResultPage[ProgramModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { p => pids(p.id) }

      override def insert(
        input: ProgramModel.Create
      ): F[ProgramModel] = {

        val create = EitherT(
          tablesRef.modify { tables =>
            val (tablesʹ, p) = input.create[State[Tables, *], Tables](TableState).run(tables).value

            p.fold(
              err => (tables,  InputError.Exception(err).asLeft),
              pm  => (tablesʹ, pm.asRight)
            )
          }
        ).rethrowT

        for {
          p <- create
          _ <- eventService.publish(ProgramEvent(_, Event.EditType.Created, p))
        } yield p
      }

      override def edit(
        input: ProgramModel.Edit
      ): F[ProgramModel] = {

        val update: State[Tables, ValidatedInput[ProgramModel]] =
          for {
            initial <- TableState.program.lookupValidated[State[Tables, *]](input.programId)
            edited   = initial.andThen(input.edit)
            _       <- edited.fold(
              _ => State.get[Tables].void,
              p => Tables.programs.mod_(progMap => progMap + (p.id -> p))
            )
          } yield edited

        for {
          p <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _ <- eventService.publish(ProgramModel.ProgramEvent.updated(p))
        } yield p
      }

    }

}
