[![Build Status](https://travis-ci.com/rufus-lang/rufus.svg?branch=master)](https://travis-ci.com/rufus-lang/rufus)
# Rufus

Rufus is a programming language for teams to build and operate fault tolerant
systems.

```rufus
module main

import "Fmt"

func main() {
    Fmt.Println("Hello, world!")
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
empty = []
numbers = [1, 2, 3, 4, 5]

map[atom]string
empty = #{}
alice = #{:name => "Alice", :age => "34"}

tuple[string, int]
empty = {}
alice = {"Alice", 34}

type Person tuple[string, int]
alice = Person{"Alice", 34}

type Person struct {
    Name string
    Age int
}
alice = Person{Name => "Alice", Age => 34}
Fmt.Printf("%s is age %d", [alice.Name, alice.Age])
```

Function types:

```rufus
func Echo(text string) string {
    text
}
```

Higher order functions:

```rufus
func Map(items list[int], fn func(int) int) list[int] {
    mapAccumulate([], items, fn)
}

func mapAccumulate(acc list[int], [h|t] list[int], f func(int) int) list[int] {
    map([f(h)|acc], t, f)
}
func mapAccumulate(acc list[int], [] list[int], func(int) int) list[int] {
    reverse(acc)
}
```

Named tuple:

```rufus
type Point tuple[X int, Y int, Z int]

point = Point{X => 2, Y => 5, Z => -1}
point.X = 2
```

Anonymous union type:

```rufus
func Teleport(point Point) :ok | Error {
    // ...
}
```

Named union type:

```rufus
type Outcome :ok | Error

func Teleport(point Point) Outcome {
    // ...
}
```

Pattern match expression:

```rufus
point = Point{X => 3, Y => -6, Z => 13}
switch Teleport(point) {
case :ok =>
    // ...
case err error.T =>
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
    mapAccumulate(list[T?]{}, n, f)
}

func mapAccumulate(acc list[T?], [h|t] list[T?], f func(T?) T?) list[T?] {
    mapAccumulate([f(h)|acc], t, f)
}
func mapAccumulate(acc list[T?], [] list[T?], func(T?) T?) list[T?] {
    lists.Reverse(acc)
}
```

The first type that binds to `T?` is substitued everywhere `T?` is mentioned.
