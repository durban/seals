/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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

import cats.{ Monad, Eval }
import cats.data.Kleisli
import cats.implicits._

final object ModelDecoding {

  sealed abstract class Proc[F[_], S] extends Monad[F] {
    def get: F[S]
    def set(s: S): F[Unit]
    def raise[A](err: String): F[A]
    def force[A](proc: F[A], s: S): Either[String, A]
    def forceAS[A](proc: F[A], s: S): (S, Either[String, A])
  }

  sealed abstract class Api {
    type F[S, A]
    implicit def procInstance[S]: Proc[F[S, *], S]
  }

  val Api: Api = Impl

  type Error = String

  final case class DecodingError(err: Error)
    extends Exception(sh"error while decoding model: ${err}")

  private final object Impl extends Api {
    override type F[S, A] = Kleisli[Eval, LocRef[S], A]
    implicit override def procInstance[S]: Proc[F[S, *], S] = {
      new Proc[F[S, *], S] {
        override def pure[A](x: A) =
          Kleisli.pure(x)
        override def flatMap[A, B](fa: Kleisli[Eval, LocRef[S], A])(
          f: A => Kleisli[Eval, LocRef[S], B]
        ) = fa.flatMap(f)
        override def tailRecM[A, B](a: A)(f: A => F[S, Either[A, B]]): F[S, B] =
          Kleisli.catsDataMonadForKleisli[Eval, LocRef[S]].tailRecM(a)(f)
        override def raise[A](err: String) =
          Kleisli { _ => Eval.always(throw new DecodingError(err)) }
        override def get =
          Kleisli { ref => ref.get }
        override def set(s: S) =
          Kleisli { ref => ref.set(s) }
        override def force[A](proc: F[S, A], s: S): Either[String, A] = {
          try Right(proc.run(new LocRef[S](s)).value)
          catch { case DecodingError(err) => Left(err) }
        }
        override def forceAS[A](proc: F[S, A], s: S): (S, Either[String, A]) = {
          val ref = new LocRef[S](s)
          val res = try {
            Right(proc.run(ref).value)
          } catch { case DecodingError(err) => Left(err) }
          (ref.get.value, res)
        }
      }
    }
  }

  private final class LocRef[A](private[this] var value: A) {
    def get: Eval[A] = Eval.always(this.value)
    def set(a: A): Eval[Unit] = Eval.always(this.value = a)
  }
}
