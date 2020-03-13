<!--

   Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
   Copyright 2020 Nokia
   SPDX-License-Identifier: Apache-2.0

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

--->

## Static compatibility checking

The library provides an instance of the `Compat[A, B]` type class,
if it can statically prove that `A` and `B` are compatible.
Compatible changes, as of now, are:
* adding a new field which has a default value
* removing a field which had a default value

Compatibility checking works recursively, that is, making a
compatible change in a leaf of an ADT allows that whole ADT
to remain compatible (and conversely, making an *in*compatible
change renders the whole ADT incompatible).

### Imports

```scala mdoc:silent
import dev.tauri.seals.Compat
```

### Example

Suppose that in the past we modeled legal persons like this:

```scala mdoc:silent
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
```

But now we want to add an address field to artificial persons too:

```scala mdoc:silent
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
```

Since `address` has no default value, this is not a compatible change:

```scala mdoc:fail
Compat[v1.LegalPerson, v2.LegalPerson] // error: could not find implicit value for ...
```

But if we define it with a default (in this case `None`):

```scala mdoc:silent
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
```

it will be compatible:

```scala mdoc
Compat[v1.LegalPerson, v11.LegalPerson]
```

### Limitations and known issues

* Order of the types must be the same for the old and new version;
  this may cause issues with `Coproduct`s (subclasses of a sealed trait).
