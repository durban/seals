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
import java.security.MessageDigest
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

private object NsUuid {

  def uuid5(ns: UUID, name: String): UUID =
    uuid5bytes(ns, ByteBuffer.wrap(name.getBytes(StandardCharsets.UTF_8)))

  private def uuid5bytes(ns: UUID, name: ByteBuffer): UUID = {
    val buf = ByteBuffer.allocate(16) // network byte order by default
    uuidToBuf(ns, buf)
    buf.rewind()
    val h = sha1()
    h.update(buf)
    h.update(name)
    val arr: Array[Byte] = h.digest().take(16)
    arr(6) = (arr(6) & 0x0f).toByte // clear version
    arr(6) = (arr(6) | 0x50).toByte // version 5
    arr(8) = (arr(8) & 0x3f).toByte // clear variant
    arr(8) = (arr(8) | 0x80).toByte // variant RFC4122
    buf.rewind()
    buf.put(arr)
    buf.rewind()
    val msl = buf.getLong()
    val lsl = buf.getLong()
    new UUID(msl, lsl)
  }

  def uuid5nested(root: UUID, names: String*): UUID =
    names.foldLeft(root)(uuid5)

  def uuid5nestedNsNm(name: String, ns1: UUID, nss: UUID*): UUID =
    uuid5(uuid5nestedNs(ns1, nss: _*), name)

  def uuid5nestedNs(ns1: UUID, nss: UUID*): UUID = {
    val buf = ByteBuffer.allocate(16)
    nss.foldLeft(ns1) { (st, u) =>
      uuidToBuf(u, buf)
      buf.rewind()
      val r = uuid5bytes(st, buf)
      buf.rewind()
      r
    }
  }

  private def uuidToBuf(u: UUID, buf: ByteBuffer): Unit = {
    buf.putLong(u.getMostSignificantBits)
    buf.putLong(u.getLeastSignificantBits)
  }

  private def sha1(): MessageDigest =
    MessageDigest.getInstance("SHA-1")
}
