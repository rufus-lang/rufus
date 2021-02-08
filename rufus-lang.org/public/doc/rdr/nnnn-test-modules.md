# Test modules

* ID: RDR-nnnn
* Status: Drafting
* Authors: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Deciders: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Date: May 5, 2020

## Context and problem statement

Packages and modules need tests to ensure that the parts function and integrate
correctly. How should test modules be organized and what relationship should
they have to the modules they exercise?

## Decision drivers

* There is an `rf test <path>` command that can run tests.
* There is a well-defined way to write test code, property-based test code,
  benchmark code, and example code.
* Users can clearly disambiguate application code from test code, and also
  clearly understand how test code relates to application code.
* Private types, constants, and functions in a module can be visible to test
  code.

## Considered options

### Option 1

Modules with names that end in `_test` are treated by the `rf` toolchain as test
modules. Functions with names that begin with `Test` are treated as tests and
run with the `rf test` command.

Example: A `Math` module that exports math-related functions is in a file called
`Math.rf`. Its tests are alongside it in a file called `Math_test.rf` containing
a module of the same name. `Math_test` is like any other module. It can import `Math` and
exercise its public API, but it doesn't have access to any of the private types,
constants or functions in `Math`.

```rufus
// Math.rf
module Math

func Sum(a int, b int) int { a + b }
func product(a int, b int) int { a * b } // This cannot be tested directly
```

```rufus
// Math_test.rf
module Math_test

import (
    "Math"
    "Testing"
)

func TestSumWithPositiveNumbers(c Testing.C) void {
    c.AssertEqual(4, Math.Sum(1, 3))
    c.AssertEqual(4, Math.Sum(2, 2))
}

func TestSumWithNegativeNumbers(c Testing.C) void {
    c.AssertEqual(-4, Math.Sum(-1, -3))
    c.AssertEqual(-4, Math.Sum(-2, -2))
}
```

This could be extended to other types of tests, such as `_bench` for benchmarks,
and `_example` for examples.

### Option 2

Modules with filenames that end in `.test.rf` are treated by the `rf` toolchain
as test modules. Functions with names that begin with `Test` are treated as
tests and run with the `rf test` command.

Example: A `Math` module that exports math-related functions is in a file called
`Math.rf`. Its tests are alongside it in a file called `Math.test.rf`, which
defines a `Math` module containing `Test*` test functions, and having access to
all private types, constants, and functions in the `Math` module defined in
`Math.rb`.

```rufus
// Math.rf
module Math

func Sum(a int, b int) int { a + b }
func product(a int, b int) int { a * b }
```

```rufus
// Math.test.rf
module Math

import (
    "Testing"
)

func TestSumWithPositiveNumbers(c Testing.C) void {
    c.AssertEqual(4, Sum(1, 3))
    c.AssertEqual(4, Sum(2, 2))
}

func TestSumWithNegativeNumbers(c Testing.C) void {
    c.AssertEqual(-4, Sum(-1, -3))
    c.AssertEqual(-4, Sum(-2, -2))
}

func TestProduct(c Testing.C) void {
    c.AssertEqual(6, product(2, 3)) // Tests have access to module internals
}
```

## Decision outcome

Chosen option: "_option 1_", because _decision tradeoffs, e.g., is a more
isolated design that can be implemented in a cleaner way than the other
options._
