Rufus is a language syntax and type system for the language semantics provided
by the BEAM.

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
- Next more than one step away from type information.

## Comments

Only single-line comments that start with `//`.

## Primitive types

- Symbols of type `atom`
  - `:value`
  - No zero value
- Booleans of type `bool`
  - `true`
  - `false`
  - Zero value is `false`
- Numbers of type `float`
  - `2.3`
  - `2.3e2`
  - `2.4e-2`
  - Zero value is `0.0`
- Numbers of type `int`
  - `value` in base 10.
  - `base#value` for integers with the base base, that must be an integer in the
    range 2..36.
  - Zero value is `0`.
- Strings of type `unicode`
  - Always encoded in UTF-8
  - `"value"`
  - Zero value is `""`
- Bitstrings and binaries of type `binary`
  - `<<10,20>>`
  - `<<"ABC">>`
  - `<<1:0,1:1>>`
  - Zero value is `<<"">>`
- Functions
  - `type Func func (args) returnType`
  - `type Func func (args) value` such as `func() :ok { :ok }` for a function
    that takes no arguments always returns the value `:ok`.
  - No zero value

## Compound types

- Tuples
  - `tuple[unicode, int]`
  - `score = tuple[unicode, int]{"red", 2}`
- Lists
  - `[]unicode`
  - `colors := []unicode{"red", "green", "blue"}
  - `list[unicode]`
  - `colors := list[unicode]{"red", "green", "blue"}
- Maps
  - `map[unicode]int`
  - `teams := map[unicode]int{"red": 2, "green": 1, "blue": 3}`
- Structs
  - `type Struct struct { fields }` such as `type Person struct { FirstName unicode, LastName unicode }`.
  - Literals like `Person{firstName: "John", lastName: "Smith"}`.

```
type retval atom
const ok = :ok
const error = :error


type PersonAge tuple {name string, age int}

pa := PersonAge{"Alice", 37}
```

## Project layout

A project contains code, dependencies and other artifacts and uses a standard
layout:

- `$projectroot/cmd/$name` - A directory containing a `main` package that will
  be built as `$projectroot/_build/bin/$name`.
- `$projectroot/src/` - A directory that contains `.rf` files with source text
  for the `$projectroot` package.

`rf new lib example` creates a new project skeleton for a library package:

```
example/
  .gitignore
  README.md
  rf.toml
  src/
```

`rf new app example` creates a new project skeleton for an app package:

```
example/
  .gitignore
  README.md
  rf.toml
  cmd/main.rf
  src/
```

`main.rf`:

```
package main

import "fmt"

func main() {
    fmt.Print("Hello, world!\n")
}
```

`rf.toml`:

```toml
[package]
name = "example"
version = "0.1.0"
authors = ["Jamu Kakar <jkakar@kakar.ca>"]

[dependencies]
"github.com/jkakar/healthcheck" = "0.2.1"
```

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

## Importing packages

```
import (
    "encoding/json"                     // stdlib imports
    "html"
    "net/http"

    "github.com/jkakar/healthcheck"     // third-party imports
    "github.com/jkakar/middleware"

    "github.com/jkakar/website/routes"  // local imports
)
```

The import path `[$(syspath)/lib, $(projectpath)/deps, $(rootpath)]` defines the
location and precedence of packages available to the compiler. When resolving a
package name to import:

1. the standard library path `$(syspath)/lib` is checked first;
2. the third-party dependencies path in `$(projectpath)/deps` is checked next;
3. finally, the local source path `$(rootpath)` is checked last. It's the
   path of the first top-level `src` directory found when traversing up the
   tree from `$(projectpath)`.

An import error occurs if the imported package can't be found in any of these
locations.

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
func All([h|t] list[T?], fn func(T) bool) bool {
    case fn(h) {
    true:
        All(t, fn)
    false:
        false
    }
}

func All([] list[T?], fn func(T) bool) bool {
    true
}
```

## Spawning a new process

A function can be run in a new process with `spawn`:

```
pid = spawn mypackage.SomeFunction(some, args)
```

That function can be shutdown, too:

```
exit(pid, :example) // No return value!
```

## Software lifecycle

Think through the lifecycle of a software project. It's created, it gets
iterated on, people have to read the code to understand how things work,
dependencies change, it must be deployed, it may be a fork, etc. The `rf` tool
should support developers from this perspective.

### 1// Inception

We need a way to quickly create a project skeleton to start new projects.

### Iteration
### Release testing
### Publication
### Transfer
