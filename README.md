[![Build Status](https://travis-ci.com/rufus-lang/rufus.svg?branch=master)](https://travis-ci.com/rufus-lang/rufus)
# Rufus

A programming language for the BEAM.

```rufus
module main

import "fmt"

func main() {
    fmt.Println("Hello, world!")
}
```

Primitive types:

```rufus
transport atom = :bicycle
truthy bool = true
answer int = 42
pi float = 3.14159265359
greeting string = "Hello, world!"
```

Constants:

```rufus
const Pi = 3.14159265359
```

Collection types:

```rufus
list[int]
numbers = list[int]{1, 2, 3, 4, 5}

map[atom]string
alice = map[atom]string{:name: "Alice", :age: "34"}

tuple[string, int]
alice = tuple[string, int]{"Alice", 34}
```

Function types:

```rufus
func Echo(text string) string {
    text
}
```

Higher order functions:

```rufus
func Map(n list[int], f func(int) int) list[int] {
    mapAccumulate(list[int]{}, n, f)
}

func mapAccumulate(acc list[int], [h|t] list[int], f func(int) int) list[int] {
    map([f(h)|acc], t, f)
}
func mapAccumulate(acc list[int], [] list[int], func(int) int) list[int] {
    lists.Reverse(acc)
}
```

Named tuple:

```rufus
type Point tuple[X int, Y int, Z int]

point = Point{X: 2, Y: 5, Z: -1}
point.X = 2
```

Union type:

```rufus
// Inline method allows shorthand syntax to help reduce boilerplate
func Teleport(point Point) :ok | tuple[:error, string] {
    // ...
}

type Outcome :ok | tuple[:error, string]

func Teleport(point Point) Outcome {
    // ...
}
```

Match expression:

```rufus
point = Point{X: 3, Y: -6, Z: 13}
match Teleport(point) {
case :ok =>
    // ...
case tuple[:error, Reason string] =>
    // ...
}
```

Each `case` branch needs to return the same (inferred) type otherwise the match
type must be declared.

```rufus
point = Point{X: 3, Y: -6, Z: 13}
match Teleport(point) Result {
case :ok =>
    // ...
case tuple[:error, Reason string] =>
    // ...
}
```

Modules and imports:

```rufus
module server

import (
    "log"
    "github.com/rufus-lang/echo"
)
```

Generic types:

```rufus
func Map(n list[T?], f func(T?) T?) list[T?] {
    map(list[T?]{}, n, f)
}

func map(acc list[T?], [h|t] list[T?], f func(T?) T?) list[T?] {
    map([f(h)|acc], t, f)
}
func map(acc list[T?], [] list[T?], func(T?) T?) list[T?] {
    lists.Reverse(acc)
}
```

The first type that binds to `T?` is substitued everywhere `T?` is mentioned.
