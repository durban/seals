/*
 * Copyright 2017 Daniel Urban
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sigs.seals
package tests

import java.math.RoundingMode

import core.EnumLike

class EnumLikeSpec extends BaseSpec {

  "EnumLike for Java enums" - {

    "java.math.RoundingMode" in {
      val inst = EnumLike[RoundingMode]
      inst.typeName should === ("java.math.RoundingMode")
      inst.name(RoundingMode.FLOOR) should === ("FLOOR")
      inst.index(RoundingMode.FLOOR) should === (RoundingMode.FLOOR.ordinal)
      inst.fromName("FLOOR") should === (Right(RoundingMode.FLOOR))
      inst.fromNameOpt("xyz") should === (None)
      inst.fromIndex(RoundingMode.FLOOR.ordinal) should === (Right(RoundingMode.FLOOR))
      inst.fromIndexOpt(RoundingMode.values().length) should === (None)
      inst.fromIndexOpt(-1) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) java.math.RoundingMode value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) java.math.RoundingMode value")
    }

    "MyTestEnum" in {
      val inst = EnumLike[MyTestEnum]
      inst.typeName should === ("io.sigs.seals.tests.MyTestEnum")
      inst.name(MyTestEnum.FOO) should === ("FOO")
      inst.index(MyTestEnum.FOO) should === (0)
      inst.fromName("FOO") should === (Right(MyTestEnum.FOO))
      inst.fromNameOpt("xyz") should === (None)
      inst.fromIndex(0) should === (Right(MyTestEnum.FOO))
      inst.fromIndexOpt(3) should === (None)
      inst.fromIndexOpt(-1) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) io.sigs.seals.tests.MyTestEnum value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) io.sigs.seals.tests.MyTestEnum value")
    }

    "MyTestEnumWithArgs" in {
      val inst = EnumLike[MyTestEnumWithArgs]
      inst.typeName should === ("io.sigs.seals.tests.MyTestEnumWithArgs")
      inst.name(MyTestEnumWithArgs.TWO) should === ("TWO")
      inst.index(MyTestEnumWithArgs.TWO) should === (1)
      inst.fromName("TWO") should === (Right(MyTestEnumWithArgs.TWO))
      inst.fromNameOpt("xyz") should === (None)
      inst.fromIndex(1) should === (Right(MyTestEnumWithArgs.TWO))
      inst.fromIndexOpt(2) should === (None)
      inst.fromIndexOpt(-1) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) io.sigs.seals.tests.MyTestEnumWithArgs value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) io.sigs.seals.tests.MyTestEnumWithArgs value")
    }

    "MyTestEnumWithOverloads" in {
      val inst = EnumLike[MyTestEnumWithOverloads]
      inst.typeName should === ("io.sigs.seals.tests.MyTestEnumWithOverloads")
      inst.name(MyTestEnumWithOverloads.FOO) should === ("FOO")
      inst.index(MyTestEnumWithOverloads.FOO) should === (0)
      inst.fromName("FOO") should === (Right(MyTestEnumWithOverloads.FOO))
      inst.fromNameOpt("xyz") should === (None)
      inst.fromIndex(0) should === (Right(MyTestEnumWithOverloads.FOO))
      inst.fromIndexOpt(2) should === (None)
      inst.fromIndexOpt(-1) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) io.sigs.seals.tests.MyTestEnumWithOverloads value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) io.sigs.seals.tests.MyTestEnumWithOverloads value")
    }

    "MyTestEnumWithToString" in {
      val inst = EnumLike[MyTestEnumWithToString]
      inst.typeName should === ("io.sigs.seals.tests.MyTestEnumWithToString")
      inst.name(MyTestEnumWithToString.FOO) should === ("FOO")
      inst.index(MyTestEnumWithToString.FOO) should === (0)
      inst.fromName("FOO") should === (Right(MyTestEnumWithToString.FOO))
      inst.fromNameOpt("x") should === (None)
      inst.fromNameOpt("y") should === (None)
      inst.fromIndex(0) should === (Right(MyTestEnumWithToString.FOO))
      inst.fromIndexOpt(456) should === (None)
      inst.fromIndexOpt(-123) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) io.sigs.seals.tests.MyTestEnumWithToString value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) io.sigs.seals.tests.MyTestEnumWithToString value")
    }
  }

  "Custom instance" in {
    import EnumLikeSpec._
    val inst = EnumLike[Switch]
    inst.index(Switch(0).fold(fail())(sw => sw)) should === (0)
    inst shouldBe theSameInstanceAs (Switch.switchEnum)
  }
}

object EnumLikeSpec {

  sealed abstract case class Switch(v: Int, s: String)
  object Switch {

    def apply(v: Int): Option[Switch] = v match {
      case 0 => Some(new Switch(0, "OFF") {})
      case 1 => Some(new Switch(1, "ON") {})
      case _ => None
    }

    implicit val switchEnum: EnumLike[Switch] = new EnumLike[Switch] {
      def typeName: String = "Switch"
      def maxIndex: Int = 1
      def name(a: Switch): String = a.s
      def fromNameOpt(name: String): Option[Switch] = name match {
        case "ON" => Switch(1)
        case "OFF" => Switch(2)
        case _ => None
      }
      def index(a: Switch): Int = a.v
      def fromIndexOpt(index: Int): Option[Switch] = Switch(index)
    }
  }
}
