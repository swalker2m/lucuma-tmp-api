// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`.StepType
import lucuma.odb.api.model.StepModel
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import sangria.schema._


object StepSchema {

  import OffsetSchema._
  import syntax.`enum`._

  implicit val EnumTypeStepType: EnumType[StepType] =
    EnumType.fromEnumerated(
      "StepType",
      "Step type"
    )

  def StepType[F[_]: Effect, A](
    typePrefix: String,
    outputType: OutputType[A]
  ): InterfaceType[OdbRepo[F], StepModel[A]] =
    InterfaceType[OdbRepo[F], StepModel[A]](
      name         = s"${typePrefix}Step",
      description  = "Step (bias, dark, science, etc.)",
      fields[OdbRepo[F], StepModel[A]](

        Field(
          name        = "stepType",
          fieldType   = EnumTypeStepType,
          description = Some("Step type"),
          resolve     = _.value.stepType
        ),

        Field(
          name        = "instrumentConfig",
          fieldType   = outputType,
          description = Some("Dynamic instrument configuration"),
          resolve     = _.value.dynamicConfig
        )

      )
    ).withPossibleTypes(() => List(
      PossibleObject[OdbRepo[F], StepModel[A]](BiasType[F, A](typePrefix, outputType)),
      PossibleObject[OdbRepo[F], StepModel[A]](DarkType[F, A](typePrefix, outputType)),
      PossibleObject[OdbRepo[F], StepModel[A]](ScienceType[F, A](typePrefix, outputType))
    ))

  def BiasType[F[_]: Effect, A](
    typePrefix:        String,
    dynamicConfigType: OutputType[A]
  ): ObjectType[OdbRepo[F], StepModel.Bias[A]] =
    ObjectType[OdbRepo[F], StepModel.Bias[A]](
      name        = s"${typePrefix}Bias",
      description = "Bias calibration step",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Bias[A]](StepType[F, A](typePrefix, dynamicConfigType))),
      fields      = Nil
    )

  def DarkType[F[_]: Effect, A](
    typePrefix:        String,
    dynamicConfigType: OutputType[A]
  ): ObjectType[OdbRepo[F], StepModel.Dark[A]] =
    ObjectType[OdbRepo[F], StepModel.Dark[A]](
      name        = s"${typePrefix}Dark",
      description = "Dark calibration step",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Dark[A]](StepType[F, A](typePrefix, dynamicConfigType))),
      fields      = Nil
    )

  def ScienceType[F[_]: Effect, A](
    typePrefix:        String,
    dynamicConfigType: OutputType[A]
  ): ObjectType[OdbRepo[F], StepModel.Science[A]] =
    ObjectType[OdbRepo[F], StepModel.Science[A]] (
      name        = s"${typePrefix}Science",
      description = "Science step",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Science[A]](StepType[F, A](typePrefix, dynamicConfigType))),
      fields      = List(

        Field(
          name        = "offset",
          fieldType   = OffsetType[F],
          description = Some("Offset"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Science[A]]) => ctx.value.offset
        )
      )

    )

}
