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

import scala.annotation.nowarn

import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{ Length, Mapper, Split }
import shapeless.ops.nat.GT._
import shapeless.ops.nat.LTEq._
import PolyDefns.{~> => ~>}

/**
 * Type class for witnessing compatibility between two types
 *
 * @tparam O The "old" version of the type
 * @tparam N The "new" version of the type
 */
trait Compat[O, N] extends Serializable

/**
 * Provides instances for `Compat`
 */
object Compat extends LowPrioCompat1 {

  type Aux[O, N] = Compat[O, N] {
    // TODO: maybe add type(s) for partial
    // (e.g., read-only) compatibility
  }

  def apply[O, N](implicit compat: Compat[O, N]): Compat.Aux[O, N] =
    compat

  /** This must be public, trust me! */
  object getSome extends (Some ~> Id) {
    def apply[A](f: Some[A]): Id[A] = f match {
      case Some(a) => a
    }
  }

  /** @note Also covers HNil and CNil */
  implicit def duh[A]: Aux[A, A] =
    new Compat[A, A] {}
}

@nowarn // lots of "unused" parameters
private[core] trait LowPrioCompat1 extends LowPrioCompat2 {

  /**
   * Derives an instance from a record (key-tagged
   * `HList` produced by `LabelledGeneric`).
   *
   * @tparam OHK Key type of the head of the old `HList`
   *         (typically a singleton Symbol type)
   * @tparam OHV Value type of the head of the old `HList`
   * @tparam OT Tail of the old `HList`
   * @tparam NHK Key type of the head of the new `HList`
   * @tparam NHV Value type of the head of the new `HList`
   * @tparam NT Tail of the new `HList`
   *
   * @param hc Evidence of compatibility between the two heads
   * @param names Evidence that the keys of the two heads match
   * @param tc Evidence of compatibility between the two tails
   */
  implicit def fromHCons[OHK, OHV, OT <: HList, NHK, NHV, NT <: HList](
    implicit
    hc: Compat[OHV, NHV],
    names: OHK =:= NHK,
    tc: Compat[OT, NT]
  ): Compat.Aux[FieldType[OHK, OHV] :: OT, FieldType[NHK, NHV] :: NT] = {
    new Compat[FieldType[OHK, OHV] :: OT, FieldType[NHK, NHV] :: NT] {}
  }

  /**
   * Derives an instance from a union (key-tagged
   * `Coproduct` produced by `LabelledGeneric`).
   *
   * @tparam OLK Key type of the left of the old `Coproduct`
   * @tparam OLV Value type of the left of the old `Coproduct`
   * @tparam OR Right of the old `Coproduct`
   * @tparam NLK Key type of the left of the new `Coproduct`
   * @tparam NLV Value type of the left of the new `Coproduct`
   * @tparam NR Right of the new `Coproduct`
   *
   * @param lc Evidence of compatibility between the two lefts
   * @param names Evidence that the keys of the two lefts match
   * @param rc Evidence of compatibility between the two rights
   */
  implicit def fromCCons[OLK, OLV, OR <: Coproduct, NLK, NLV, NR <: Coproduct](
    implicit
    lc: Compat[OLV, NLV],
    names: OLK =:= NLK,
    rc: Compat[OR, NR]
  ): Compat.Aux[FieldType[OLK, OLV] :+: OR, FieldType[NLK, NLV] :+: NR] = {
    new Compat[FieldType[OLK, OLV] :+: OR, FieldType[NLK, NLV] :+: NR] {}
  }
}

@nowarn // lots of "unused" parameters
private[core] trait LowPrioCompat2 {

  /**
   * Derives an instance for types which have a `Coproduct`
   * as their generic representation (e.g., sealed traits).
   *
   * @tparam O The old type (typically a sealed trait)
   * @tparam GO The generic representation of `O`
   * @tparam N The new type
   * @tparam GN The generic representation of `N`
   *
   * @param og `LabelledGeneric` for `O`
   * @param ng `LabelledGeneric` for `N`
   * @param comp Evidence of compatibility between the
   *        two `Coproducts` (@see `fromCCons`)
   */
  implicit def fromGenericCoproduct[O, GO <: Coproduct, N, GN <: Coproduct](
    implicit
    og: LabelledGeneric.Aux[O, GO],
    ng: LabelledGeneric.Aux[N, GN],
    comp: Lazy[Compat[GO, GN]]
  ): Compat.Aux[O, N] = new Compat[O, N] {}

  /**
   * Derives an instance for types which have a `HList`
   * as their generic representation (e.g., case classes).
   *
   * Compatibility allows added or removed fields, but
   * all these must have default values.
   *
   * @tparam O The old type (typically a case class)
   * @tparam N The new type
   * @tparam TL The type which have more fields (i.e.,
   *         its `HList` representation is longer),
   *         either `O` or `N` (@see `LongerRepr`)
   * @tparam TS The type which have fewer fields (i.e.,
   *         its `HList` representation is shorter),
   *         either `N` or `O`
   * @tparam GL Generic representation of `TL`
   * @tparam GS Generic representation of `TS`
   * @tparam DL Defaults for `TL` (@see `shapeless.Default`)
   * @tparam DS Defaults for `TS`
   * @tparam L Length of the shorter `HList` (i.e., `GS`)
   * @tparam GSS The prefix of `GL` which has
   *         the same length as `GS` (i.e., `L`)
   * @tparam GNF Generic representation of the
   *         new/removed fields
   *         @note GL ≡ GSS ++ GNF
   * @tparam DSS The prefix of `DL` which has
   *         the same length as `DS` (i.e., `L`)
   * @tparam DNF Defaults for the new/removed fields
   *         @note DL ≡ DSS ++ DNF
   *
   * @param longer Selects the longer and shorter types
   *        (`TL`, `TS`) and also provides their generic
   *        representation (`GL`, `GS`)
   * @param longerDefaults Default values for the longer type
   * @param shorterDefaults Default values for the shorter type
   * @param lengthOfShorter Computes the length of the shorter type
   * @param splitRepr Splits `GL` at `L`, thus computing `GSS` and `GNF`
   * @param splitDefaults Splits `DL` at `L`, thus computing `DSS` and `DNF`
   * @param map Extracts the type of all defaults from `DNF`, thus
   *        guaranteeing that every new/removed field has a default
   * @param comp Evidence of compatibility between the overlapping
   *        fields of the two types (i.e., `GS` and `GSS`)
   * @param compDefaults Evidence of compatibility between the
   *        overlapping defaults
   *        FIXME: do we need this? I.e., can it happen, that the
   *        default types are not exactly the same as the field types?
   */
  implicit def fromGenericProductNewOrRemovedFields[
    O, N,
    TL, TS,
    GL <: HList, GS <: HList,
    DL <: HList, DS <: HList,
    L <: Nat,
    GSS <: HList, GNF <: HList,
    DSS <: HList, DNF <: HList
  ](implicit
    longer: LongerRepr.Aux[O, N, TL, GL, TS, GS],
    longerDefaults: Default.Aux[TL, DL],
    shorterDefaults: Default.Aux[TS, DS],
    lengthOfShorter: Length.Aux[GS, L],
    splitRepr: Split.Aux[GL, L, GSS, GNF],
    splitDefaults: Split.Aux[DL, L, DSS, DNF],
    map: Mapper[Compat.getSome.type, DNF],
    comp: Lazy[Compat[GS, GSS]],
    compDefaults: Lazy[Compat[DS, DSS]]
  ): Compat.Aux[O, N] = new Compat[O, N] {}

  /**
   * This is only needed to provide the `compDefaults`
   * argument of `fromGenericProductNewOrRemovedFields`.
   * TODO: maybe we should hide this from the API
   */
  implicit def fromNonLabelledHCons[OH, OT <: HList, NH, NT <: HList](
    implicit
    hc: Compat[OH, NH],
    tc: Compat[OT, NT]
  ): Compat.Aux[OH :: OT, NH :: NT] = new Compat[OH :: OT, NH :: NT] {}
}

/**
 * Helper type class for selecting the
 * longer and shorter of two case classes
 * (or similar types)
 *
 * @tparam A The first type
 * @tparam B The second type
 */
trait LongerRepr[A, B] {

  /**
   * The longer of the two types
   * (either `A` or `B`). If they
   * have the same length, it's `A`.
   */
  type Longer

  /**
   * The `LabelledGeneric` representation
   * of the longer type (`Longer`)
   */
  type LongerRepr <: HList

  /**
   * The shorter of the two types
   * (either `B` or `A`). If they
   * have the same length, it's `B`.
   */
  type Shorter

  /**
   * The `LabelledGeneric` representation
   * of the shorter type (`Shorter`)
   */
  type ShorterRepr <: HList
}

/**
 * Provides instances for `LongerRepr`
 */
@nowarn // lots of "unused" parameters
object LongerRepr extends LowPrioLongerRepr {

  type Aux[A, B, L, LR <: HList, S, SR <: HList] = LongerRepr[A, B] {
    type Longer = L
    type LongerRepr = LR
    type Shorter = S
    type ShorterRepr = SR
  }

  def apply[A, B](
    implicit l: LongerRepr[A, B]
  ): Aux[A, B, l.Longer, l.LongerRepr, l.Shorter, l.ShorterRepr] = l

  /**
   * Provides an instance if `B` is longer
   */
  implicit def secondIsLonger[A, GA <: HList, AL <: Nat, B, GB <: HList, BL <: Nat](
    implicit
    aLg: LabelledGeneric.Aux[A, GA],
    bLg: LabelledGeneric.Aux[B, GB],
    aLen: Length.Aux[GA, AL],
    bLen: Length.Aux[GB, BL],
    gt: BL > AL
  ): LongerRepr.Aux[A, B, B, GB, A, GA] = new LongerRepr[A, B] {
    type Longer = B
    type LongerRepr = GB
    type Shorter = A
    type ShorterRepr = GA
  }
}

@nowarn // lots of "unused" parameters
private[core] trait LowPrioLongerRepr extends Serializable {

  /**
   * Provides an instance if `A` is longer,
   * of they have an equal length
   */
  implicit def firstIsLongerOrEq[A, GA <: HList, AL <: Nat, B, GB <: HList, BL <: Nat](
    implicit
    aLg: LabelledGeneric.Aux[A, GA],
    bLg: LabelledGeneric.Aux[B, GB],
    aLen: Length.Aux[GA, AL],
    bLen: Length.Aux[GB, BL],
    le: BL <= AL
  ): LongerRepr.Aux[A, B, A, GA, B, GB] = new LongerRepr[A, B] {
    type Longer = A
    type LongerRepr = GA
    type Shorter = B
    type ShorterRepr = GB
  }
}
