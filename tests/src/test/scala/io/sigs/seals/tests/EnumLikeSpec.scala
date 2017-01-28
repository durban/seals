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
      inst.fromName("FLOOR") should === (Some(RoundingMode.FLOOR))
      inst.fromName("xyz") should === (None)
      inst.fromIndex(RoundingMode.FLOOR.ordinal) should === (Some(RoundingMode.FLOOR))
      inst.fromIndex(RoundingMode.values().length) should === (None)
      inst.fromIndex(-1) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) java.math.RoundingMode value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) java.math.RoundingMode value")
    }

    "MyTestEnum" in {
      val inst = EnumLike[MyTestEnum]
      inst.typeName should === ("io.sigs.seals.tests.MyTestEnum")
      inst.name(MyTestEnum.FOO) should === ("FOO")
      inst.index(MyTestEnum.FOO) should === (0)
      inst.fromName("FOO") should === (Some(MyTestEnum.FOO))
      inst.fromName("xyz") should === (None)
      inst.fromIndex(0) should === (Some(MyTestEnum.FOO))
      inst.fromIndex(3) should === (None)
      inst.fromIndex(-1) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) io.sigs.seals.tests.MyTestEnum value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) io.sigs.seals.tests.MyTestEnum value")
    }

    "MyTestEnumWithArgs" in {
      val inst = EnumLike[MyTestEnumWithArgs]
      inst.typeName should === ("io.sigs.seals.tests.MyTestEnumWithArgs")
      inst.name(MyTestEnumWithArgs.TWO) should === ("TWO")
      inst.index(MyTestEnumWithArgs.TWO) should === (1)
      inst.fromName("TWO") should === (Some(MyTestEnumWithArgs.TWO))
      inst.fromName("xyz") should === (None)
      inst.fromIndex(1) should === (Some(MyTestEnumWithArgs.TWO))
      inst.fromIndex(2) should === (None)
      inst.fromIndex(-1) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) io.sigs.seals.tests.MyTestEnumWithArgs value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) io.sigs.seals.tests.MyTestEnumWithArgs value")
    }

    "MyTestEnumWithOverloads" in {
      val inst = EnumLike[MyTestEnumWithOverloads]
      inst.typeName should === ("io.sigs.seals.tests.MyTestEnumWithOverloads")
      inst.name(MyTestEnumWithOverloads.FOO) should === ("FOO")
      inst.index(MyTestEnumWithOverloads.FOO) should === (0)
      inst.fromName("FOO") should === (Some(MyTestEnumWithOverloads.FOO))
      inst.fromName("xyz") should === (None)
      inst.fromIndex(0) should === (Some(MyTestEnumWithOverloads.FOO))
      inst.fromIndex(2) should === (None)
      inst.fromIndex(-1) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) io.sigs.seals.tests.MyTestEnumWithOverloads value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) io.sigs.seals.tests.MyTestEnumWithOverloads value")
    }

    "MyTestEnumWithToString" in {
      val inst = EnumLike[MyTestEnumWithToString]
      inst.typeName should === ("io.sigs.seals.tests.MyTestEnumWithToString")
      inst.name(MyTestEnumWithToString.FOO) should === ("FOO")
      inst.index(MyTestEnumWithToString.FOO) should === (0)
      inst.fromName("FOO") should === (Some(MyTestEnumWithToString.FOO))
      inst.fromName("x") should === (None)
      inst.fromName("y") should === (None)
      inst.fromIndex(0) should === (Some(MyTestEnumWithToString.FOO))
      inst.fromIndex(456) should === (None)
      inst.fromIndex(-123) should === (None)
      inst.fromNameError("xyz") should === ("'xyz' is not a(n) io.sigs.seals.tests.MyTestEnumWithToString value")
      inst.fromIndexError(-1) should === ("-1 is not a(n) io.sigs.seals.tests.MyTestEnumWithToString value")
    }
  }
}
