# rf

`rf` provides tools for working with Rufus programs.

## Build

    `rebar3 escriptize`

## Run

    `_build/default/bin/rf`

## Code layout

- The `rf` module defines the escript that provides the `rf` command-line
  interface.
- Modules that start with `rfc_` are part of the Rufus compiler.
