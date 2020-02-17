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

    compile             Compile source code and then run it and print its output
    debug:erlang-forms  Print Erlang source code from a file as abstract forms
    debug:parse         Parse source code and print parse forms
    debug:rufus-forms   Print Rufus source code from a file as abstract forms
    debug:tokenize      Scan source code and print parse tokens
    version             Print Rufus version

Use "rf help [command]" for more information about that command

Additional help topics:

    spec                Language specification

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
