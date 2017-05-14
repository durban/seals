/*
 * Copyright 2016-2017 Daniel Urban
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

/**
 * This is an ugly workaround for SI-7046,
 * regarding `Model` and some of its subclasses.
 */
@deprecated("SI-7046 workaround", since = "forever")
private final class AnSi7046Workaround {

  def `This is to force Composite`: Model.Composite[_, _] =
    sys.error("This should never be called")

  def `This is to force HCons`: Model.HCons[_] =
    sys.error("This should never be called")

  def `This is to force CCons`: Model.CCons =
    sys.error("This should never be called")

  def `And this is to force Vector`: Model.Vector =
    sys.error("This should never be called")
}
