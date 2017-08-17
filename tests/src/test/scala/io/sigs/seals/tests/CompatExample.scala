/*
 * Copyright 2016 Daniel Urban and contributors listed in AUTHORS
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
package tests

import scala.concurrent.duration.Duration

import shapeless.test.illTyped

object CompatExample {

  object events {

    /** Original version */
    object v1 {

      sealed trait Event

      sealed trait RegistrationEvent extends Event
      final case class Registered(name: String, age: Int = 0)
        extends RegistrationEvent
      final case class Deregistered(id: Long)
        extends RegistrationEvent

      sealed trait OfferEvent extends Event
      final case class Offer(id: Long, user: Long, amount: BigDecimal)
        extends OfferEvent
      final case class OfferRescinded(id: Long)
        extends OfferEvent

      sealed trait ConnectionEvent extends Event
      final case class Connected(sessionId: Long)
        extends ConnectionEvent
      final case object Disconnected
        extends ConnectionEvent
    }

    /**
     * Changes since `v1`:
     * - `age` field removed from `Registered`
     * - `timeout` field added to `Connected`
     */
    object v2 {

      sealed trait Event

      sealed trait RegistrationEvent extends Event
      final case class Registered(name: String)
        extends RegistrationEvent
      final case class Deregistered(id: Long)
        extends RegistrationEvent

      sealed trait OfferEvent extends Event
      final case class Offer(id: Long, user: Long, amount: BigDecimal)
        extends OfferEvent
      final case class OfferRescinded(id: Long)
        extends OfferEvent

      sealed trait ConnectionEvent extends Event
      final case class Connected(sessionId: Long, timeout: Duration = Duration.Zero)
        extends ConnectionEvent
      final case object Disconnected
        extends ConnectionEvent
    }
  }

  object legalPerson {

    object v1 {
      sealed trait LegalPerson
      final case class NaturalPerson(
        name: String,
        address: String
      ) extends LegalPerson
      final case class ArtificialPerson(
        legalName: String,
        established: Int
      ) extends LegalPerson
    }

    object v11 {
      sealed trait LegalPerson
      final case class NaturalPerson(
        name: String,
        address: String
      ) extends LegalPerson
      final case class ArtificialPerson(
        legalName: String,
        established: Int,
        address: Option[String] = None // new field with default
      ) extends LegalPerson
    }

    object v2 {
      sealed trait LegalPerson
      final case class NaturalPerson(
        name: String,
        address: String
      ) extends LegalPerson
      final case class ArtificialPerson(
        legalName: String,
        established: Int,
        address: Option[String] // new field
      ) extends LegalPerson
    }

    object v3 {
      sealed trait LegalPerson
      final case class NaturalPerson(
        name: String,
        address: String
      ) extends LegalPerson
      final case class ArtificialPerson(
        legalName: String,
        est: Int, // renamed field
        address: Option[String]
      ) extends LegalPerson
    }
  }
}

class CompatExample extends BaseSpec {

  import CompatExample._

  "Events example" in {
    import events._
    Compat[v1.Event, v2.Event]
    Compat[v2.Event, v1.Event]
  }

  "Legal person example" in {
    import legalPerson._
    Compat[v1.LegalPerson, v11.LegalPerson]
    Compat[v11.LegalPerson, v1.LegalPerson]
    illTyped("Compat[v1.LegalPerson, v2.LegalPerson]", notFound)
    illTyped("Compat[v2.LegalPerson, v1.LegalPerson]", notFound)
    illTyped("Compat[v2.LegalPerson, v3.LegalPerson]", notFound)
    illTyped("Compat[v3.LegalPerson, v2.LegalPerson]", notFound)
    illTyped("Compat[v1.LegalPerson, v3.LegalPerson]", notFound)
    illTyped("Compat[v3.LegalPerson, v1.LegalPerson]", notFound)
  }
}
