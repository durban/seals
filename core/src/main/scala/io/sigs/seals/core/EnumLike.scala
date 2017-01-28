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

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

// TODO: change to fromName(name: String): Either[String, A] (and fromIndex too)
// TODO: could cache the result of .values()
trait EnumLike[A] extends Serializable {
  def typeName: String
  def name(a: A): String
  def index(a: A): Int
  def fromName(name: String): Option[A]
  def fromIndex(index: Int): Option[A]
  def fromNameError(s: String): String =
    s"'${s}' is not a(n) ${typeName} value"
  def fromIndexError(i: Int): String =
    s"${i} is not a(n) ${typeName} value"
}

object EnumLike {

  def apply[A](implicit inst: EnumLike[A]): EnumLike[A] =
    inst

  implicit def enumLikeJavaEnum[A <: java.lang.Enum[A]]: EnumLike[A] =
    macro enumLikeJavaEnumImpl[A]

  // TODO: add abstract class to generate less code
  def enumLikeJavaEnumImpl[A](c: Context)(implicit A: c.WeakTypeTag[A]): c.Expr[EnumLike[A]] = {
    import c.universe._
    val companion = weakTypeOf[A].typeSymbol.companion.asTerm
    val tree = q"""
      (new _root_.io.sigs.seals.core.EnumLike[${A}] {

        final override def typeName: _root_.scala.Predef.String =
          ${A.tpe.toString}

        final override def name(a: ${A}): _root_.scala.Predef.String =
          a.name()

        final override def index(a: ${A}): _root_.scala.Int =
          a.ordinal()

        final override def fromName(name: _root_.scala.Predef.String): _root_.scala.Option[${A}] = {
          try {
            _root_.scala.Some[${A}](${companion}.valueOf(name))
          } catch { case _: _root_.scala.IllegalArgumentException => _root_.scala.None }
        }

        final override def fromIndex(index: _root_.scala.Int): _root_.scala.Option[${A}] = {
          val arr: _root_.scala.Array[${A}] = ${companion}.values()
          if ((index >= 0) && (index < arr.length)) _root_.scala.Some[${A}](arr(index))
          else _root_.scala.None
        }
      }) : _root_.io.sigs.seals.core.EnumLike[${A}]
    """
    c.Expr[EnumLike[A]](tree)
  }
}
