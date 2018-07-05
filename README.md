A programming language for the BEAM.

## Design goals

- Approachable and teachable.
- Encourage a consistent programming style.
- Scale up to support many people working on very large software projects.
- Scale down to support small experiments.
- Provide a high-level concurrency model.
- Good performance.
- No surprises about where functions and data come from.
- Type safe to prevent bugs and support refactoring.
- Data over types.
- Straight-forward tooling.

## Comments

Only single-line comments that start with `//`.

## Primitive types

- Symbols of type `atom`
  - `:value`
- Booleans of type `bool`
  - `true`
  - `false`
- Numbers of type `float`
  - `2.3`
  - `2.3e2`
  - `2.4e-2`
- Numbers of type `int`
  - `value` in base 10.
  - `base#value` for integers with the base base, that must be an integer in the
    range 2..36.
- Strings of type `unicode`
  - Always encoded in UTF-8
  - `"value"`
- Bitstrings and binaries of type `binary`
  - `<<10,20>>`
  - `<<"ABC">>`
  - `<<1:0,1:1>>`
- Functions
  - `type Func func (args) returnType`
  - `type Func func (args) value` such as `func() :ok { :ok }` for a function
    that takes no arguments always returns the value `:ok`.

## Compound types

- Maps
  - `map[string]int`
- Lists
  - `[]string`
- Structs
  - `type Struct struct { fields }` such as `type Person struct { firstName string, lastName string }`.
  - Literals like `Person{firstName: "John", lastName: "Smith"}`.
- Tuples
  - `type Tuple tuple { fields }` such as `type Person tuple { string, string }`.
  - Literals like `Person{"John", "Smith"}`.

## Packages

All code lives in a package. Packages have lowercase names and don't use
underscores.

```
package list
```

Identifiers that start with a capital letter are exported. Everything else is
private.

```
package math

const Pi float := 3.141519
```

A readonly `math.Pi` value that can be used by packages that import the `math`
package.

```
package thing

func validate(name string) bool {
    // ...
}
```

`validate` is a private function that may only be called by code within the
`thing` package.

## Functions

All functions must have a return type. Functions can be tail recursive:

```
func InfiniteLoop(n int) {
    InfiniteLoop(n+1)
}
```

## Generics

Types that end in a `?` are generic and are instantiated at compile time.

```
func All([h|t] []T?, fn func(T) bool) bool {
    case fn(h) {
    true:
        All(t, fn)
    false:
        false
    }
}

func All([] []T?, fn func(T) bool) bool {
    true
}
```
