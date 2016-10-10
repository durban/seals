package io.sigs.seals
package circe

import cats.kernel.Eq
import cats.instances.all._

import org.scalacheck.Arbitrary

import io.sigs.seals.laws.ArbInstances
import io.sigs.seals.laws.TestArbInstances
import io.sigs.seals.laws.TestTypes

class CirceLawsSpec extends tests.BaseLawsSpec {

  import EnvelopeCodec._
  import ArbInstances.arbEnvelope
  import TestArbInstances.forTestData._

  checkParametricLaws[Int]("Int")
  checkParametricLaws[String]("String")
  checkParametricLaws[Vector[String]]("Vector[String]")
  checkParametricLaws[TestTypes.adts.defs.Adt1]("Adt1")
  checkParametricLaws[TestTypes.adts.recursive.IntList]("IntList")
  checkParametricLaws[List[TestTypes.adts.recursive.IntList]]("List[IntList]")
  checkParametricLaws[TestTypes.collections.Adt]("collections.Adt")

  def checkParametricLaws[A](name: String)(implicit a: Arbitrary[A], eq: Eq[A], r: Reified[A]): Unit = {
    checkAll(s"Envelope[$name].CirceLaws", claws.CirceLaws[Envelope[A]].roundtrip)
  }
}
