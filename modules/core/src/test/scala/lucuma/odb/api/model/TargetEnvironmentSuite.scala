// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb.ArbTargetEnvironmentModel

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

/**
 *
 */
final class TargetEnvironmentSuite extends DisciplineSuite {

  import ArbTargetEnvironmentModel._

  checkAll("TargetEnvironmentModel", EqTests[TargetEnvironmentModel].eqv)
  checkAll("TargetEnvironmentModel.Create", EqTests[TargetEnvironmentModel.CreateTargetEnvironmentInput].eqv)
  checkAll("TargetEnvironmentModel.Edit", EqTests[TargetEnvironmentModel.EditTargetEnvironmentInput].eqv)

}
