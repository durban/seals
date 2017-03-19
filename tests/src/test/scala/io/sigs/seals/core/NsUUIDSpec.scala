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
package core

import java.util.UUID

import scala.util.{ Try, Success, Failure }

import io.circe._

import _root_.scodec.bits._

class NsUUIDSpec extends tests.BaseSpec {

  /** Version 1 UUID */
  val ns1 = uuid"8f07c16c-f14d-11e6-81af-1d121b157edb"
  assert(ns1.variant() === 2)
  assert(ns1.version() === 1)

  /** Version 3 (name-based MD5) UUID for "example.com" (DNS) */
  val ns3 = uuid"9073926b-929f-31c2-abc9-fad77ae3e8eb"
  assert(ns3.variant() === 2)
  assert(ns3.version() === 3)

  /** Version 4 (random) UUID as test namespace */
  val ns4 = uuid"3b4ff1b0-5235-47b9-bdff-8f7df19bf8a4"
  assert(ns4.variant() === 2)
  assert(ns4.version() === 4)

  /** Version 5 (name-based SHA1) UUID for "example.com" (DNS) */
  val ns5 = uuid"cfbff0d1-9375-5685-968c-48ce8b15ae17"
  assert(ns5.variant() === 2)
  assert(ns5.version() === 5)

  /** DNS namespace from RFC 4122 */
  val nsDns = uuid"6ba7b810-9dad-11d1-80b4-00c04fd430c8"
  assert(nsDns.variant() === 2)

  /** URL namespace from RFC 4122 */
  val nsUrl = uuid"6ba7b811-9dad-11d1-80b4-00c04fd430c8"
  assert(nsUrl.variant() === 2)

  "Basic functionality" - {

    "empty name" in {
      NsUUID.uuid5(ns4, "") should === (uuid"56756e5d-8a7e-570f-a419-82ea6d431713")
    }

    "short name" in {
      NsUUID.uuid5(ns4, "alpha") should === (uuid"21bbb574-bba8-51e4-8b71-2ab43a593184")
    }

    "long name" in {
      NsUUID.uuid5(ns4, "the quick brown fox jumps over the lazy dog") should === (
        uuid"e9e3506b-5eca-5b3b-916f-c9d8fdce37c8"
      )
    }
  }

  "Various namespaces" - {

    "DNS" in {
      NsUUID.uuid5(nsDns, "example.com") should === (ns5)
    }

    "URL" in {
      NsUUID.uuid5(nsUrl, "http://www.example.com/a/b/c") should === (
        uuid"c3d9ade2-286d-5034-ab44-93d660958179"
      )
    }

    "v1" in {
      NsUUID.uuid5(ns1, "foobar") should === (uuid"d247cb15-9aff-5df1-beff-fdbc144f042a")
    }

    "v3" in {
      NsUUID.uuid5(ns3, "foobar") should === (uuid"ae857671-99d7-5c5c-b458-c95c071bc730")
    }

    "v5" in {
      NsUUID.uuid5(ns5, "foobar") should === (uuid"f1030914-4615-533a-ba0f-ce2603a31662")
    }
  }

  "Nested namespaces" - {

    val n1 = uuid"d71eb6ce-094e-47d1-8a87-0fe592905d05"
    val n2 = uuid"75f91432-77d8-4ab3-a9c4-2a2652878029"
    val n3 = uuid"e3c836b9-ac3c-4cc9-8ff6-b208515deda8"
    val n4 = uuid"4458e30e-8120-47fc-a325-39053796fd83"

    "UUIDs and a name" in {
      val name = "foobar"
      NsUUID.uuid5nestedNsNm(name, ns1) should === (NsUUID.uuid5(ns1, name))
      NsUUID.uuid5nestedNsNm(name, ns1, n1) should === (uuid"8fc121a2-bdb6-57fd-9f4b-8c57d9860d7d")
      NsUUID.uuid5nestedNsNm(name, ns1, n1, n2, n3, n4) should === (uuid"7f8c26c6-d014-58cc-a205-25c13c2b98c0")
    }

    "names" in {
      NsUUID.uuid5nested(ns1) should === (ns1)
      NsUUID.uuid5nested(ns1, "foo") should === (uuid"37af6235-cf58-51f3-8a67-3e6a0eedff96")
      NsUUID.uuid5nested(ns1, "foo", "bar", "baz") should === (uuid"fd8f5430-b2d5-5d2b-8524-57da37991e36")
    }

    "UUIDs" in {
      NsUUID.uuid5nestedNs(ns1) should === (ns1)
      NsUUID.uuid5nestedNs(ns1, n1) should === (uuid"da3145fc-debf-5024-be13-051b8a1217d2")
      NsUUID.uuid5nestedNs(ns1, n1, n2, n3, n4) should === (uuid"cd7b7bd8-3810-5be5-9c6f-05c8dc1bb8c6")
    }

    "generated test data" in {
      val str = stringFromResource("/test_data.json")
      val json = io.circe.parser.parse(str).fold(err => fail(err.toString), x => x)
      checkFromJsonData(json)
    }
  }

  "UUIDBuilder" - {

    val root = uuid"2fffa6dc-d430-4c8a-9d90-a53764159e89"

    val u1 = uuid"ec11c15f-7137-4531-a862-8c72b71fd8d4"
    val u2 = uuid"676f5566-97a3-47b4-8ab5-3fd6a3f00b61"

    "empty" in {
      UUIDBuilder(root).uuid should === (
        NsUUID.uuid5bv(root, ByteVector.empty)
      )
    }

    "with UUIDs" in {
      (root / u1 / u2).uuid should === (
        NsUUID.uuid5bv(root, ByteVector.fromUUID(u1) ++ ByteVector.fromUUID(u2))
      )
    }

    "with ByteVectors" in {
      (root / hex"deadbeef" / hex"abcdef").uuid should === (
        NsUUID.uuid5bv(root, hex"deadbeef abcdef")
      )
    }

    "with Strings" in {
      (root / "xyz" / "1256hgds").uuid should === (
        NsUUID.uuid5bv(root, ByteVector.encodeUtf8("xyz1256hgds").fold(
          err => fail(err.toString),
          bv => bv
        ))
      )
    }

    "with mixed" in {
      (root / u1 / hex"abef" / "éáű" / u2).uuid should === (
        NsUUID.uuid5bv(
          root,
          ByteVector.fromUUID(u1) ++
            hex"abef" ++
            ByteVector.encodeUtf8("éáű").fold(
              err => fail(err.toString),
              bv => bv
            ) ++
            ByteVector.fromUUID(u2)
        )
      )
    }
  }

  def checkFromJsonData(j: Json): Unit = {
    def go(j: Json, nss: Vector[Either[UUID, String]]): Unit = {
      j.as[Map[String, Json]] match {
        case Left(err) =>
          // reached a leaf:
          j.as[UUID].fold(err => fail(s"not an UUID: ${j} (${err})"), _ => ())
        case Right(map) =>
          for ((k, v) <- map) {
            Try(UUID.fromString(k)) match {
              case Success(uuid) =>
                go(v, nss :+ Left(uuid))
              case Failure(_) =>
                if (k.isEmpty) {
                  // must've reached an UUID:
                  v.as[UUID].fold(
                    err => fail(s"expected UUID at empty key, got '${v}' (${err})"),
                    uuid => composite(nss) should === (uuid)
                  )
                } else {
                  go(v, nss :+ Right(k))
                }
            }
          }
      }
    }

    def composite(nss: Vector[Either[UUID, String]]): UUID = nss match {
      case Left(uuid) +: t => comp(uuid, t)
      case _ => fail("root must be a UUID")
    }

    def comp(root: UUID, nss: Vector[Either[UUID, String]]): UUID = {
      val (u, _) = nss.foldLeft((root, true)) { (st, us) =>
        (st, us) match {
          case ((s, true), Left(uuid)) => (NsUUID.uuid5nestedNs(s, uuid), true)
          case ((s, f), Right(name)) => (NsUUID.uuid5(s, name), false)
          case ((_, false), Left(_)) => fail("UUID after name")
        }
      }
      u
    }

    go(j, Vector.empty)
  }

  def stringFromResource(res: String): String = {
    val stream = this.getClass.getResourceAsStream(res)
    try {
      new java.util.Scanner(stream, "UTF-8").useDelimiter("\\A").next()
    } finally {
      stream.close()
    }
  }
}
