# Organizing code

- Module dependencies should be explicit.
- It should be clear where dependencies come from and where to get updates.
- Unused dependencies are not allowed.
- Unused variables are not allowed.
- There is one way to publish and install packages.
- Packages contain modules and act as a unit of distribution.
- Packages are decentralized by default.
- Provide a package index service.
- Modules provide namespaces for types, constants and functions.

## Overview

In Rufus, types, constants and functions are written in modules, which provide
namespaces and group related code together. Modules are imported when needed and
the elements within them are referenced by name. For example, this code imports
the `github.com/rufus-lang/rufus/example` module and uses it to invoke the
`Echo(text string) string` function and print its result with `fmt.Println` from
the standard library:

```rufus
module main

import (
    "fmt"
    "github.com/rufus-lang/rufus/example"
)

func main() {
    fmt.Println(example.Echo("Hello, world!"))
}
```

## Packages

A package contains one or more Rufus modules along with information about
dependencies. Packages are typically version controlled and have a standard file
layout:

```
.git                  # Git repository metadata
.gitignore            # Git ignore list
rufus.config          # Rufus config file
rufus.lock            # Rufus lock file
bin/
    example           # Built executable
cmd/example/
pkg/                  # Package dependencies
src/                  # Program source code
    echo.rf
    echo_test.rf
src/bin/              # Executable source code
    example.rf
```

All source code goes in `src/`. Executable source code goes in `src/bin/`. Built
executables are named based on the source filename and written to `bin/`.
Package dependencies are downloaded and stored in `pkg/`.

The `rufus.config` file contains important details about the package:

```
[package]
name = example
version = 0.8.3
package_root = github.com/rufus-lang/example
authors = ["Jamu Kakar <jkakar@kakar.ca>"]
```

## Dependencies

Dependencies are declared in the `rufus.config` file.

```
[package]
name = example
version = 0.8.3
package_root = github.com/rufus-lang/example
authors = ["Jamu Kakar <jkakar@kakar.ca>"]

[dependencies]
"github.com/aws/aws-sdk-rufus" = 4.3
```

Running `rf build` will fetch new dependencies and all other transitive
dependencies, compile them, and update `rufus.lock`. Dependencies are downloaded
into the `pkg/` directory:

```
pkg/
    github.com/aws/aws-sdk-rufus/
    github.com/jkakar/logfmt/
```

Each of these directories contains the entire contents of the packages at those
URLs. Rufus resolves imports by first trying to match the import path to the
standard library. If that doesn't match it attempts to match the package root.
If that doesn't match, the dependencies in `pkg/` will be traversed to find a
match.

## Modules

A module provides a namespace for types, constants and functions.

## Service index

A UX for creating a new package like:

- Load `https://packages.rufus-lang.org`.
- Paste a GitHub URL into a box and click the _Register_ button.
- Land on package page that includes information about the freshness of the
  data.
- Asynchronously, the index fetches information about versions by looking at
  tags in the repository. Tags that match the form `vN.N.N` where `N >= 0`.

Once registered, the index will periodically look for new versions. When it
finds them, it'll automatically track and make them available in search results
and on the package page.

