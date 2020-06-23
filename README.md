[![Build Status](https://travis-ci.com/rufus-lang/rufus.svg?branch=main)](https://travis-ci.com/rufus-lang/rufus)
# Rufus

Rufus is a programming language for people that build and operate fault tolerant
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
empty = list[int]{}
numbers = list[int]{1, 2, 3, 4, 5}
list[int]{head|tail} = numbers

map[atom]string
empty = map[atom]string{}
alice = map[atom]string{:name => "Alice", :age => "34"}

set[int]
empty = set[int]{}
primes = set[int]{2, 3, 5, 7}

tuple[string,int]
alice = tuple[string,int]{"Alice", 34}

type Person tuple[string,int]
alice = Person{"Alice", 34}

type Person struct {
    Name string
    Age  int
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
    mapOver(list[int]{}, items, fn)
}

func mapOver(acc list[int], list[int]{head|tail}, fn func(int) int) list[int] {
    map(list[int]{fn(head)|acc}, tail, fn)
}
func mapOver(acc list[int], list[int]{}, func(int) int) list[int] {
    List.Reverse(acc)
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

Generics:

```rufus
func Map(items list[T?], fn func(T?) T?) list[T?] {
    mapOver(list[T?]{}, items, fn)
}

func mapOver(acc list[T?], list[T?]{head|tail}, fn func(T?) T?) list[T?] {
    mapOver(list[T?]{fn(head)|acc}, tail, fn)
}
func mapOver(acc list[T?], list[T?]{}, func(T?) T?) list[T?] {
    List.Reverse(acc)
}
```

The first type that binds to `T?` is substitued everywhere `T?` is mentioned.
