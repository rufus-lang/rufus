# Erlang compatibility

* ID: RDR-nnnn
* Status: Drafting
* Authors: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Deciders: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Date: June 23, 2020

## Context and problem statement

What syntax should Rufus use to call into Erlang? How should type checking work
with Erlang libraries?

## Decision drivers

* It's possible to integrate Erlang libraries, including behaviors they define,
  into Rufus in a way that follows Rufus conventions.
* It's possible to represent types for the full range of function heads that
  exist for a given Erlang function.
* Type signatures for Erlang functions do not have to be declared by the caller.

## Considered options

### Option 1

Follow the pattern that Elixir users and make it possible to invoke functions on
atoms that map to Erlang module names:

```rufus
module Math

func Sin(x float) float {
    :math.sin(x)
}
```

In this example, the compiler can infer the return type of `:math.sin/1` based
on the return type of the `Sin` function, but in other cases, the type will need
to be declared:

```rufus
func Sin(x float) float {
    n float = :math.sin(x)
    n
}
```

Some functions require generics:

```rufus
module Map

func Get[type K, V](collection map[K]V, key K, default V) V {
    :maps.get(key, collection, default)
}
```

```rufus
items = map[string]int{"a": 1, "b": 2}
value = Map.Get(items, "a", -1)
```

### Option 2

## Decision outcome

Chosen option: option _N_ because ...
