# Exported names

* Status: Drafting
* Deciders: jkakar@kakar.ca <<jkakar@kakar.ca>>
* Date: n/a

## Context and problem statement

A package should only export public modules, and a module should only export
public types, constants and functions. It should not be possible for a user to
access private symbols in a package they're using. What facilities should Rufus
provide for users to maintain encapsulation?

## Decision drivers

* A user can tell whether a symbol is public or private at a glance.
* It's not possible to access private modules from outside their package.
* It's not possible to access private types, constants, or functions from
  outside their module.

## Considered options

### Option 1

Names that start with an `_` or a lowercase letter in the range `a-z` are
private. All other names are public. Privacy rules apply to the types,
constants, and functions defined in modules.

A public module called `math` with a public `Pi` constant and a private `float`
constant:

```rufus
module math

const Pi float = 3.14159265359
const quarter float = 0.25
```

It would be used exterally like:

```
module example

import package/path/math

func Area(radius tuple{:radius, int}) float {
    math.Pow(math.Pi * radius, 2)
}
```

Modules that have a directory called `internal` in their package path are only
accessible within the package.

### Option 2

Extend the approach in Option 1 to include module names. Modules that start with
an `_` or a lowercase letter are only accessible within the package. All other
modules are externally accessible. This makes the privacy mechanism uniform and
eliminates the need to special-case `internal` directories.

A public module called `Math` with a public `Pi` constant and a private `float`
constant:

```rufus
module Math

const Pi float = 3.14159265359
const quarter float = 0.25
```

It would be used exterally like:

```
module example

import package/path/Math

func Area(radius tuple{:radius, int}) float {
    Math.Pow(math.Pi * radius, 2)
}
```

## Decision outcome

Chosen option: option 2, because it uses one unified pattern instead of both the
case-sensitive approach and special-case handling of the `internal` directory.
It also has the benefit of making private modules more obvious when reading
code.
