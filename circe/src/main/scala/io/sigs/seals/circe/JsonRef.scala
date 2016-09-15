/*
 * Copyright 2016 Daniel Urban
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
package circe

import scala.util.hashing.MurmurHash3
import io.circe._
import cats.data.Xor
import cats.implicits._

private final class JsonRef private (val path: Vector[String])
    extends Product // FIXME: do we need this?
    with Serializable {

  lazy val uri: String = {
    if (path.isEmpty) {
      "#"
    } else {
      s"#/${path.map(JsonRef.escape).mkString("/")}"
    }
  }

  lazy val asJson: Json =
    JsonRef.jsonRefEncoder(this)

  lazy val asJsonObject: JsonObject =
    JsonRef.jsonRefEncoder.encodeObject(this)

  def dropRight(n: Int): JsonRef =
    JsonRef(path.dropRight(n))

  override def equals(that: Any): Boolean = that match {
    case JsonRef(thatPath) =>
      this.path === thatPath
    case _ =>
      false
  }

  override def hashCode: Int =
    MurmurHash3.seqHash(path)

  override def canEqual(that: Any): Boolean = that match {
    case JsonRef(_) => true
    case _ => false
  }

  override def productArity: Int =
    1

  override def productElement(n: Int): Any = n match {
    case 0 => path
    case _ => throw new IndexOutOfBoundsException
  }
}

private object JsonRef {

  final val key = s"$$ref"

  def apply(uri: String): Xor[String, JsonRef] = {
    for {
      u <- Xor.catchNonFatal(java.net.URI.create(uri)).bimap(_.getMessage, identity)
      p <- checkUri(u)
    } yield JsonRef(p)
  }

  // FIXME: maybe use Validated?
  private def checkUri(uri: java.net.URI): Xor[String, Vector[String]] = {
    Option(uri.getFragment).fold[Xor[String, Vector[String]]] {
      Xor.Left("no fragment")
    } { frag =>
      if (notNullOrEmpty(uri.getHost)) {
        Xor.Left("nonempty host")
      } else if (notNullOrEmpty(uri.getPath)) {
        Xor.Left("nonempty path")
      } else if (uri.getPort =!= -1) {
        Xor.Left("nonempty port")
      } else if (notNullOrEmpty(uri.getAuthority)) {
        Xor.Left("nonempty authority")
      } else if (notNullOrEmpty(uri.getQuery)) {
        Xor.Left("nonempty query")
      } else if (notNullOrEmpty(uri.getScheme)) {
        Xor.Left("nonempty scheme")
      } else if (notNullOrEmpty(uri.getUserInfo)) {
        Xor.Left("nonempty user info")
      }
      else {
        if (frag.isEmpty) {
          Xor.Right(Vector.empty)
        } else if (frag.startsWith("/")) {
          Xor.Right(frag.slice(1, frag.length).split("/", -1).toVector.map(unescape))
        } else {
          Xor.Left(s"illegal JSON pointer: '$frag'")
        }
      }
    }
  }

  private[this] def notNullOrEmpty(s: String): Boolean =
    Option(s).getOrElse("").nonEmpty

  def apply(path: Vector[String]): JsonRef = {
    new JsonRef(path)
  }

  private def escape(s: String): String = {
    val x = s.replaceAll("~", "~0")
    x.replaceAll("/", "~1")
  }

  private def unescape(s: String): String = {
    val x = s.replaceAll("~1", "/")
    x.replaceAll("~0", "~")
  }


  def fromCursor(cur: Cursor): JsonRef = {
    val path = cur.context.foldRight[List[String]](Nil) { (ctx, path) =>
      val pathComp = ctx.fold(identity, _.toString)
      pathComp :: path
    }

    JsonRef(path.reverse.toVector)
  }

  def unapply(ref: JsonRef): Some[Vector[String]] =
    Some(ref.path)

  implicit val jsonRefEncoder: ObjectEncoder[JsonRef] = new ObjectEncoder[JsonRef] {
    override def encodeObject(a: JsonRef): JsonObject =
      JsonObject.singleton(JsonRef.key, Json.fromString(a.uri))
  }

  implicit val jsonRefDecoder: Decoder[JsonRef] = new Decoder[JsonRef] {
    override def apply(c: HCursor): Decoder.Result[JsonRef] = {
      for {
        uri <- c.get[String](key)
        res <- JsonRef(uri).bimap(
          msg => DecodingFailure(msg, c.history),
          identity
        )
      } yield res
    }
  }
}
