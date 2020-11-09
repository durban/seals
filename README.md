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

# seals

[![Latest version](https://index.scala-lang.org/durban/seals/seals-core/latest.svg)](
https://index.scala-lang.org/durban/seals/seals-core)

[*seals*](https://github.com/durban/seals) is an experimental project
by [Daniel Urban](https://github.com/durban), which provides tools for
**s**chema **e**volution **a**nd **l**anguage-integrated **s**chemata.

By using it, you will be able to

- define schemata by creating ordinary Scala datatypes;
- check the compatibility of different versions of a schema
  at compile time, as well as runtime;
- automatically derive serializers and deserializers for your
  schema-datatypes (currently only [circe] and [scodec] encoders
  and decoders are implemented);
- and communicate between components using different (but compatible)
  versions of a schema.

Since it's a fairly new project, not all of these features are
implemented yet. Bugs are to be expected as well.
[Contributions](CONTRIBUTING.md) to improve the project, and reports about
[issues](https://github.com/durban/seals/issues) you encounter are welcome.

## Getting started

*seals* is currently available for Scala 2.12 and 2.13. JARs are available
on [Maven Central](https://search.maven.org/search?q=g:dev.tauri%20seals).
To use it, put this into your `build.sbt` (see [below](#project-structure)
for the available modules):

```scala
libraryDependencies += "dev.tauri" %% "seals-core" % "0.4.0-M3"
```

All releases (and commits on the `master` branch) are signed by key
`36A8 2002 483A 4CBF A5F8 DF6F 48B2 9573 BF19 7B13`.

## Features

### Defining schemata

By using `seals-core`, you can define a schema simply by creating an ADT:

```scala mdoc:silent
final case class User(id: Int, name: String)
```

An abstract representation of this schema can be retrieved by requesting
an instance of the `Reified` type class.

```scala mdoc
import dev.tauri.seals.Reified
Reified[User]
```

This abstract representation is used to implement the following features.
(End users usually don't have to work with `Reified` directly.)

### Compile-time compatibility checking

In the next version of the schema defined above, you may want to add a new field
(with a default value):

```scala mdoc:silent
final case class UserV2(id: Int, name: String, age: Int = 42)
```

Thanks to the default value, these two versions are compatible with
each other. We can assert this by using the `Compat` type class:

```scala mdoc
import dev.tauri.seals.Compat
Compat[User, UserV2]
```

If they wouldn't be compatible, we would get a *compile time* error
(because there would be no `Compat` instance available). For example,
if we define a new schema like this:

```scala mdoc:silent
final case class UserV3(id: Int, name: String, age: Int) // no default `age`
```

Then there will be no `Compat` instance available, since the schemata
are not compatible:

```scala mdoc:fail
Compat[User, UserV3] // error: could not find implicit value for ...
```

For a more detailed introduction to the `Compat` type class,
see [this example](core/src/main/tut/Compat.md).

### Build-time compatibility checking

By using the `seals-plugin` module (which is an sbt plugin), we can
check in our build whether our current schemata are compatible with
previously released versions. (Similarly to how [MiMa] checks binary
compatibility with previous versions.) For how to do this, see
[this example](plugin/src/sbt-test/seals-plugin/example). The plugin
is available for sbt 1.x.

### Other features

If you are interested in other features (like automatic derivation of
serializers, or runtime compatibility checking), at the moment
the best way is to look at the [examples](examples) or directly
at the sources (and Scaladoc comments, and laws/tests).

## Project structure

The subprojects are as follows:

- [`core`](core): essential type classes (required)
- [`circe`](circe): automatic derivation of [circe]
  encoders and decoders (optional)
- [`scodec`](scodec): automatic derivation of [scodec]
  codecs, encoders and decoders (optional)
- [`refined`](refined): support for [refined] types
- [`plugin`](plugin): sbt plugin for build-time compatibility
  checking of schema definitions (basically [MiMa] for schemata)
- [`checker`](checker): the schema checker used by the sbt plugin
- [`macros`](macros): a few macros used internally by `core`
- [`laws`](laws): definitions of laws for the type classes in `core` (incomplete, for testing)
- [`tests`](tests): unittests (don't depend on this)
- [`examples`](examples): a few examples for using the library

## Dependencies

*seals* depends on the following projects:

- [shapeless](https://github.com/milessabin/shapeless) provides
  the macros and type classes to automatically derive schemata
  and other type class instances for ADTs.
- [Cats](https://github.com/typelevel/cats) provides general
  functional programming tools which complement the Scala standard library.
- [scodec-bits](https://github.com/scodec/scodec-bits) provides an
  immutable `ByteVector` datatype.
- [MiMa] provides some utilities for working with artifacts in `seals-plugin`.

Currently there are interop modules for the following projects:

- [circe] provides the JSON framework for which `seals` derives encoders and decoders.
- [scodec] provides a binary encoding/serialization framework for which `seals` derives codecs.
  - [FS2](https://github.com/functional-streams-for-scala/fs2) enables streaming encoding
    and decoding of data with scodec.
- [refined] provides refinement types, some of which are supported by `seals`.

For testing, it also uses:

- [ScalaTest](https://github.com/scalatest/scalatest) for the unittests,
- [ScalaCheck](https://github.com/rickynils/scalacheck) for automated
  property-based testing,
- and [scalacheck-shapeless](https://github.com/alexarchambault/scalacheck-shapeless)
  to generate pseudorandom ADT instances.

## License

*seals* is open source software under the [Apache License v2.0](https://www.apache.org/licenses/LICENSE-2.0).
For details, see the [LICENSE.txt](LICENSE.txt), [NOTICE.txt](NOTICE.txt), and [AUTHORS](AUTHORS) files.

[circe]: https://github.com/circe/circe
[scodec]: https://github.com/scodec/scodec
[refined]: https://github.com/fthomas/refined
[MiMa]: https://github.com/typesafehub/migration-manager
