# Exported identifiers

* ID: RDR-nnnn
* Status: Drafting
* Authors: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Deciders: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Date: May 3, 2019

## Context and problem statement

A package should only export public modules, and a module should only export
public types, constants and functions. It should not be possible for a user to
access private identifiers in a package they're using. What facilities should
Rufus provide for users to manage encapsulation?

## Decision drivers

* A user can tell whether an identifier is public or private at a glance.
* Works well for all users, including those that uses non-latin languages and
  alphabets.
* It's not possible to access private modules from outside their package.
* It's not possible to access private types, constants, or functions from
  outside their module.

## Considered options

### Option 1

Idenifiers for types, constants, and functions defined in the module block that
start with an `_` or a Unicode lower case letter (Unicode class "Ll") are
private. All other identifiers for types, constants, and functions are exported.

Example: A public module called `math` with a public `Pi` constant and a private
`quarter` constant, both `float`s:

```rufus
module math

const Pi float = 3.14159265359
const quarter float = 0.25
```

It would be used exterally like:

```rufus
module example

import "math"

func Area(:radius, r float) float {
    math.Pow(math.Pi * r, 2)
}
```

Modules that have a directory called `internal` in their package path are only
accessible within the package.

### Option 2

Extend the approach in Option 1 to include module identifiers. Modules that
start with an `_` or a lowercase letter are only accessible within the package.
All other modules are externally accessible. This makes the privacy mechanism
uniform and eliminates the need to special-case `internal` directories.

Example: A public module called `Math` with a public `Pi` constant and a private
`quarter` constant:

```rufus
module Math

const Pi float = 3.14159265359
const quarter float = 0.25
```

It would be used externally like:

```rufus
module example

import "Math"

func Area(:radius, r float) float {
    Math.Pow(Math.Pi * r, 2)
}
func Area(:rectangle, tuple{length, width float}) float {
    length * width
}
func Area(:square, side float) float {
    Math.Pow(side, 2)
}
```

An issue with this approach is the possibility of collisions on case-insensitive
filesystems. A private `math` module in `math.rf` could conflict with a public
`Math` module in `Math.rf`, for example.

## Decision outcome

Chosen option: option 2, because it uses one unified pattern instead of two
different patterns. It also has the benefit of making private modules more
obvious when reading code because they start with an `_` or a lowercase letter.
We will fail a build if two or more Rufus source files in the same directory
have names that differ only by case. This will prevent collision issues on
case-insensitive filesystems.

Open question: will a leading `_` for module, type, constant, or function cause
parser grammer conflicts with `_` in pattern matching syntax and semantics?
