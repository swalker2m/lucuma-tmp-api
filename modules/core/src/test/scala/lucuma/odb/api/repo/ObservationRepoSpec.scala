// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{ObservationModel, ProgramModel}

import cats.syntax.all._
import cats.kernel.instances.order._
import clue.data.Input
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Prop.forAll
import munit.ScalaCheckSuite

final class ObservationRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  import arb.ArbTables._

  private def randomSelect(
    tables: Tables,
    indices: List[Int]
  ): List[ObservationModel] = {
      val size: Int =
        tables.observations.size

      val keep: Set[Int]            =
        if (size === 0) Set.empty[Int] else indices.map(i => (i % size).abs).toSet

      tables.observations.zipWithIndex.collect {
        case ((_, o), i) if keep(i) => o
      }.toList.sortBy(_.id)

  }

  property("selectPageForObservations") {
    forAll { (t: Tables, indices: List[Int]) =>

      val expected = randomSelect(t, indices).filter(_.existence.isPresent).map(_.id)
      val obtained = runTest(t) { _.observation.selectPageForObservations(expected.toSet) }.nodes.map(_.id)

      assertEquals(obtained, expected)
    }
  }

  property("selectPageForObservations with deleted") {
    forAll { (t: Tables, indices: List[Int]) =>

      val expected = randomSelect(t, indices).map(_.id)
      val obtained = runTest(t) { _.observation.selectPageForObservations(expected.toSet, includeDeleted = true) }.nodes.map(_.id)

      assertEquals(obtained, expected)
    }
  }

  property("selectPageForObservations with first") {
    forAll { (t: Tables, indices: List[Int], first: Int) =>

      val limitedFirst = if (t.observations.size === 0) 0 else (first % t.observations.size).abs

      val expected = randomSelect(t, indices).filter(_.existence.isPresent).map(_.id)
      val obtained = runTest(t) { _.observation.selectPageForObservations(expected.toSet, count = limitedFirst.some ) }.nodes.map(_.id)

      assertEquals(obtained, expected.take(limitedFirst))
    }
  }

  private def runEditTest(
    t: Tables
  )(
    f: ObservationModel => ObservationModel.Edit
  ): (ObservationModel, ObservationModel) =

    runTest(t) { odb =>
      for {
        // Insert a program and observation to insure that at least one exists
        p  <- odb.program.insert(ProgramModel.Create(None, None, None))
        _  <- odb.observation.insert(ObservationModel.Create.empty(p.id))

        // Pick whatever the first observation may be
        tʹ    <- odb.tables.get
        before = tʹ.observations.values.head

        // Do the prescribed edit.
        after <- odb.observation.edit(f(before))
      } yield (before, after)
    }

  property("simple edit") {

    forAll { (t: Tables) =>
      val (_, obs) = runEditTest(t) { o =>
        ObservationModel.Edit(o.id, name = Input(NonEmptyString.unsafeFrom("Biff")))
      }
      assert(obs.name.contains(NonEmptyString.unsafeFrom("Biff")))
    }

  }

  property("simple non-edit") {
    forAll { (t: Tables) =>
      val (before, after) = runEditTest(t) { o =>
        ObservationModel.Edit(o.id, name = o.name.fold(Input.ignore[NonEmptyString])(n => Input(n)))
      }
      assertEquals(after, before)
    }

  }

}
