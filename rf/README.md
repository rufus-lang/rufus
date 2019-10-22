[![Build Status](https://travis-ci.com/rufus-lang/rufus.svg?branch=master)](https://travis-ci.com/rufus-lang/rufus)
# rf

`rf` provides tools for working with Rufus programs.

## Build

Run the tests and build an `rf` binary:

```
make
```

## Run

There's a single `rf` escript command that contains subcommands for all Rufus
tools.

```
rf
```
```
rf provides tools for working with Rufus programs.

Usage:

    rf COMMAND [OPTION]...

The commands are:

    compile:core-erlang   parse source code and print Core Erlang translation
    compile:parse         parse source code and print AST
    compile:scan          scan source code and print tokens
    version               print Rufus version

Use "rf help [command]" for more information about that command

Additional help topics:

    spec            language specification

Use "rf help [topic]" for more information about that topic
```

## Test

Run the tests:

```
make check
```

Sometimes eunit show an exception failure without a stack trace, which makes
debugging tedious. You can use a test shell to run the failing tests by hand to
see the stack trace:

https://stackoverflow.com/questions/34658714/how-to-debug-erlang-code-during-rebar3-eunit

## Code layout

- The `rf` module defines the escript that provides the `rf` command-line
  interface.
- Modules that start with `rufus_` are part of the Rufus toolchain.
