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
package laws

trait Serialization {

  def roundtripSer[A](a: A): A = {
    val bs = new java.io.ByteArrayOutputStream
    val os = new java.io.ObjectOutputStream(bs)
    val bytes = try {
      os.writeObject(a)
      os.close()
      bs.close()
      bs.toByteArray()
    } finally {
      os.close()
      bs.close()
    }

    val is = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bytes)) {

      // This is a workaround, because deserialization
      // sometimes fails to find some classes. (See also SI-9777.)

      private[this] val cl = Thread.currentThread().getContextClassLoader

      protected override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
        try {
          Class.forName(desc.getName, false, cl)
        } catch {
          case cnf: ClassNotFoundException =>
            super.resolveClass(desc)
        }
      }
    }

    try {
      is.readObject().asInstanceOf[A]
    } finally {
      is.close()
    }
  }
}
