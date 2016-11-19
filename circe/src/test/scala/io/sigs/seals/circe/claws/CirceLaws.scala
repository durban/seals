package io.sigs.seals
package circe
package claws

import cats.kernel.laws._
import cats.kernel.Eq

import io.circe._
import io.circe.syntax._

import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

object CirceLaws {
  def apply[A](implicit a: Arbitrary[A], e: Eq[A]): CirceLaws[A] = new CirceLaws[A] {
    def Arb = a
    def Equ = e
  }
}

trait CirceLaws[A] extends Laws {

  implicit def Arb: Arbitrary[A]
  implicit def Equ: Eq[A]

  def roundtrip(implicit enc: Encoder[A], dec: Decoder[A]) = new CirceRuleSet(
    name = "roundtrip",
    parent = None,
    "encode-then-decode" -> forAll { (a: A) =>
      a.asJson.as[A] match {
        case Right(r) =>
          r ?== a
        case Left(err) =>
          Prop(Result(status = False)) :| {
            err.toString
          }
      }
    }
  )

  final class CirceRuleSet(
    val name: String,
    val parent: Option[this.RuleSet],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val bases = Nil
  }
}
