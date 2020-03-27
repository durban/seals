/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
 * Copyright 2020 Nokia
 * SPDX-License-Identifier: Apache-2.0
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

package dev.tauri.seals
package core

import cats.implicits._

trait ExtMap[F[_, _], K, V] extends Serializable {

  /** Unique keys, not necessarily sorted key-value pairs */
  def toVector(fa: F[K, V]): Vector[(K, V)]

  /** Must return `None` iff there are duplicate keys */
  def fromVector(v: Vector[(K, V)]): Option[F[K, V]]
}

final object ExtMap {

  def apply[F[_, _], K, V](implicit inst: ExtMap[F, K, V]): ExtMap[F, K, V] =
    inst

  def instance[F[_, _], K, V](
    toVect: F[K, V] => Vector[(K, V)],
    fromVect: Vector[(K, V)] => Option[F[K, V]]
  ): ExtMap[F, K, V] = {
    new ExtMap[F, K, V] {
      override def toVector(fa: F[K, V]): Vector[(K, V)] =
        toVect(fa)
      override def fromVector(v: Vector[(K, V)]): Option[F[K, V]] =
        fromVect(v)
    }
  }

  implicit def extMapForMap[K, V]: ExtMap[Map, K, V] = instance(
    _.toVector,
    { vec =>
      val map = vec.toMap
      if (map.size === vec.size) Some(map)
      else None
    }
  )
}
