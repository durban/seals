/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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
package macros

import scala.reflect.macros.blackbox.Context

import cats.implicits._

trait EnumLike[A] extends Serializable {

  def typeName: String

  def maxIndex: Int

  def name(a: A): String

  def fromNameOpt(name: String): Option[A]

  def fromName(name: String): Either[String, A] =
    Either.fromOption(fromNameOpt(name), ifNone = fromNameError(name))

  def fromNameError(s: String): String =
    show"'${s}' is not a(n) ${typeName} value"

  def index(a: A): Int

  def fromIndexOpt(index: Int): Option[A]

  def fromIndex(index: Int): Either[String, A] =
    Either.fromOption(fromIndexOpt(index), ifNone = fromIndexError(index))

  def fromIndexError(i: Int): String =
    show"${i} is not a(n) ${typeName} value"
}

object EnumLike {

  abstract class JavaEnum[A <: java.lang.Enum[A]]()
      extends EnumLike[A] {

    final override def name(a: A): String =
      a.name

    final override def index(a: A): Int =
      a.ordinal
  }

  def apply[A](implicit inst: EnumLike[A]): EnumLike[A] =
    inst

  implicit def enumLikeJavaEnum[A <: java.lang.Enum[A]]: EnumLike[A] =
    macro enumLikeJavaEnumImpl[A]

  def enumLikeJavaEnumImpl[A](c: Context)(implicit A: c.WeakTypeTag[A]): c.Expr[EnumLike[A]] = {
    import c.universe._
    val companion = weakTypeOf[A].typeSymbol.companion.asTerm
    val tree = q"""
      (new _root_.dev.tauri.seals.core.EnumLike.JavaEnum[${A}] {

        private[this] final val values: _root_.scala.Array[${A}] =
          ${companion}.values()

        final override val maxIndex: _root_.scala.Int =
          values.length - 1

        final override def typeName: _root_.scala.Predef.String =
          ${A.tpe.toString}

        final override def fromNameOpt(name: _root_.scala.Predef.String): _root_.scala.Option[${A}] = {
          try {
            _root_.scala.Some[${A}](${companion}.valueOf(name))
          } catch {
            case _: _root_.scala.IllegalArgumentException => _root_.scala.None
          }
        }

        final override def fromIndexOpt(index: _root_.scala.Int): _root_.scala.Option[${A}] = {
          if ((index >= 0) && (index <= maxIndex)) _root_.scala.Some[${A}](values(index))
          else _root_.scala.None
        }
      }) : _root_.dev.tauri.seals.core.EnumLike[${A}]
    """
    c.Expr[EnumLike[A]](tree)
  }
}
