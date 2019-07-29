Educe
====================

[![Build Status](https://travis-ci.org/magiclen/educe.svg?branch=master)](https://travis-ci.org/magiclen/educe)

This crate provides procedural macros to help you implement Rust-build-in traits quickly.

## Debug

Use `#[derive(Educe)]` and `#[educe(Debug)]` to implement the `Debug` trait for a struct, an enum, or a union. It supports to change the name of your types, variants and fields. You can also ignore some fields, or set a trait and/or a method to replace the `Debug` trait used by default. Also, you can even format a struct to a tuple, and vice versa.

#### Basic Usage

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Debug)]
struct Struct {
    f1: u8
}

#[derive(Educe)]
#[educe(Debug)]
enum Enum {
    V1,
    V2 {
        f1: u8,
    },
    V3(u8),
}
```

#### Change the Name of a Type, a Variant or a Field

The `name` attribute can help you rename a type, a variant or a field.

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Debug(name = "Struct2"))]
struct Struct {
    #[educe(Debug(name = "f"))]
    f1: u8
}

#[derive(Educe)]
#[educe(Debug(name = true))]
enum Enum {
    #[educe(Debug(name = false))]
    V1,
    #[educe(Debug(name = "V"))]
    V2 {
        #[educe(Debug(name = "f"))]
        f1: u8,
    },
    #[educe(Debug(name = false))]
    V3(u8),
}
```

#### Ignore Fields

The `ignore` attribute can ignore specific fields.

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Debug)]
struct Struct {
    #[educe(Debug(ignore))]
    f1: u8
}

#[derive(Educe)]
#[educe(Debug)]
enum Enum {
    V1,
    V2 {
        #[educe(Debug(ignore))]
        f1: u8,
    },
    V3(
        #[educe(Debug(ignore))]
        u8
    ),
}
```

#### Fake Structs and Tuples

With the `named_field` attribute, structs can be formatted as tuples and tuples can be formatted as structs.

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Debug(named_field = false))]
struct Struct {
    f1: u8
}

#[derive(Educe)]
#[educe(Debug)]
enum Enum {
    V1,
    #[educe(Debug(named_field = false))]
    V2 {
        f1: u8,
    },
    #[educe(Debug(named_field = true))]
    V3(
        u8,
        #[educe(Debug(name = "value"))]
        i32
    ),
}
```

#### Use Another Method or Trait to Do the Format Thing

The `format` attribute has two parameters: `trait` and `method`. They can be used to replace the `Debug` trait on fields. If you only set the `trait` parameter, the `method` will be set to `fmt` automatically by default.

```rust
#[macro_use] extern crate educe;

use std::fmt::{self, Formatter};

fn fmt(_s: &u8, f: &mut Formatter) -> fmt::Result {
    f.write_str("Hi")
}

trait A {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("Hi")
    }
}

impl A for i32 {};
impl A for u64 {};

#[derive(Educe)]
#[educe(Debug)]
enum Enum<T: A> {
    V1,
    V2 {
        #[educe(Debug(format(method = "fmt")))]
        f1: u8,
    },
    V3(
        #[educe(Debug(format(trait = "std::fmt::UpperHex")))]
        u8,
        #[educe(Debug(format(trait = "A")))]
        T
    ),
}
```

#### Generic Parameters Bound to the `Debug` Trait or Others

The `#[educe(Debug(bound))]` attribute can be used to add the `Debug` trait bound to all generaic parameters for the `Debug` implementation.

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Debug(bound))]
enum Enum<T, K> {
    V1,
    V2 {
        f1: K,
    },
    V3(
        T
    ),
}
```

Or you can set the where predicates by yourself.

```rust
#[macro_use] extern crate educe;

use std::fmt::{self, Formatter};

fn fmt(_s: &u8, f: &mut Formatter) -> fmt::Result {
    f.write_str("Hi")
}

trait A {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("Hi")
    }
}

impl A for i32 {};
impl A for u64 {};

#[derive(Educe)]
#[educe(Debug(bound = "T: std::fmt::Debug, K: A"))]
enum Enum<T, K> {
    V1,
    V2 {
        #[educe(Debug(format(trait = "A")))]
        f1: K,
    },
    V3(
        T
    ),
}
```

#### Union

A union will be formatted to a `u8` slice, because we don't know it's field at runtime. The fileds of a union cannot be ignored, renamed or formated with other methods or traits.

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Debug)]
struct Union {
    f1: u8,
    f2: i32,
}
```

## Hash

Use `#[derive(Educe)]` and `#[educe(Hash)]` to implement the `Hash` trait for a struct or an enum. It supports to ignore some fields, or set a trait and/or a method to replace the `Hash` trait used by default.

#### Basic Usage

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Hash)]
struct Struct {
    f1: u8
}

#[derive(Educe)]
#[educe(Hash)]
enum Enum {
    V1,
    V2 {
        f1: u8,
    },
    V3(u8),
}
```

#### Ignore Fields

The `ignore` attribute can ignore specific fields.

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Hash)]
struct Struct {
    #[educe(Hash(ignore))]
    f1: u8
}

#[derive(Educe)]
#[educe(Hash)]
enum Enum {
    V1,
    V2 {
        #[educe(Hash(ignore))]
        f1: u8,
    },
    V3(
        #[educe(Hash(ignore))]
        u8
    ),
}
```

#### Use Another Method or Trait to Do Hashing

The `hash` attribute has two parameters: `trait` and `method`. They can be used to replace the `Hash` trait on fields. If you only set the `trait` parameter, the `method` will be set to `hash` automatically by default.

```rust
#[macro_use] extern crate educe;

use std::hash::{Hash, Hasher};

fn hash<H: Hasher>(_s: &u8, state: &mut H) {
    Hash::hash(&100, state)
}

trait A {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&100, state)
    }
}

impl A for i32 {};
impl A for u64 {};

#[derive(Educe)]
#[educe(Hash)]
enum Enum<T: A> {
    V1,
    V2 {
        #[educe(Hash(hash(method = "hash")))]
        f1: u8,
    },
    V3(
        #[educe(Hash(hash(trait = "A")))]
        T
    ),
}
```

#### Generic Parameters Bound to the `Hash` Trait or Others

The `#[educe(Hash(bound))]` attribute can be used to add the `Hash` trait bound to all generaic parameters for the `Hash` implementation.

```rust
#[macro_use] extern crate educe;

#[derive(Educe)]
#[educe(Hash(bound))]
enum Enum<T, K> {
    V1,
    V2 {
        f1: K,
    },
    V3(
        T
    ),
}
```

Or you can set the where predicates by yourself.

```rust
#[macro_use] extern crate educe;

use std::hash::{Hash, Hasher};

fn hash<H: Hasher>(_s: &u8, state: &mut H) {
    Hash::hash(&100, state)
}

trait A {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&100, state)
    }
}

impl A for i32 {};
impl A for u64 {};

#[derive(Educe)]
#[educe(Hash(bound = "T: std::hash::Hash, K: A"))]
enum Enum<T, K> {
    V1,
    V2 {
        #[educe(Hash(hash(trait = "A")))]
        f1: K,
    },
    V3(
        T
    ),
}
```

## TODO

There is a lot of work to be done. Unimplemented traits are listed below:

1. `Default`
1. `Clone`
1. `Copy`
1. `PartialEq`
1. `Eq`
1. `PartialOrd`
1. `Ord`
1. `From`
1. `Into`
1. `FromStr`
1. `TryFrom`
1. `Deref`
1. `DerefMut`

## Crates.io

https://crates.io/crates/educe

## Documentation

https://docs.rs/educe

## License

[MIT](LICENSE)
