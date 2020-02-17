# List syntax

* ID: RDR-nnnn
* Status: Drafting
* Authors: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Deciders: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Date: February 14, 2020

## Context and problem statement

What syntax should Rufus use for list types, literals, and cons expressions?

## Decision drivers

* The syntax should be consistent, especially between literal and cons
  representations, and avoid unnecessary boilerplate where doing so helps
  readability.
* The syntax should work well in match expressions as well as patterns in
  function heads.
* The syntax should sit cleanly alongside tuples and maps (and eventually sets).
* The syntax should scale to custom types.
* Parsing and typechecking the syntax should be straightforward to reduce
  complexity in the compiler and ensure that other tools, such as `rf fmt`, can
  easily work with the syntax.

## Considered options

Collection types will use a uniform syntax:

- `list[int]`
- `tuple[int,int]`
- `map[string]int`
- `set[float]`

`[]int` is a common syntax shape for lists, and has the benefit of being a touch
shorter, but `list[int]` is symmetrical with the expected syntax for `tuple` and
`map` types.

### Option 1

The first option uses the tried-and-true square bracket syntax for lists, and
also employs a `<param> <type>` shape for patterns in function heads:

```rufus
func Empty() list[int] { [] }
func Echo(numbers list[int]) list[int] { numbers }

func Sum(numbers list[int]) int { sum(0, numbers) }
func sum(total int, [] list[int]) { total }
func sum(total int, [head|tail] list[int]) { sum(total + head, tail) }

func Prepend(v int) list[int] { [v|[2, 3, 4]] }
func LossyCopy([] list[int]) { [] }
func LossyCopy(numbers list[int]) {
    [_|tail] = numbers
    tail
}

func Multidimensional() list[list[string]] {
    [["hotdog"], ["milk", "coffee"]]
}

type Numbers list[int]

func Echo(numbers Numbers) Numbers { numbers }
func Empty() Numbers { [] }
func Numbers() Numbers { [1, 2, 3, 4] }
func Numbers([head|tail] Numbers) Numbers { [head|[1|tail]] }
```

This syntax has some nice properties. It's regular, and it's also familiar, but
it also has some downsides. Notably, assigning a type to `[]` is difficult. The
compiler could treat this type as `list[<any>]` until the first time an element
is added to it, but that would break the current assumption that expressions
_always_ have a well-defined type.

### Option 2

The second option merges the type with the expression, and also makes the list
type explicit in almost all cases:

```rufus
func Empty() list[int] { list[int]{} }
func Echo(numbers list[int]) list[int] { numbers }

func Sum(numbers list[int]) int { sum(0, numbers) }
func sum(acc int, list[int]{}) { total }
func sum(acc int, list[int]{head|tail}) { sum(acc + head, tail) }

func Prepend(v int) list[int] { list[int]{v|{2, 3, 4}} }
func LossyCopy(list[int]{}) { list[int]{} }
func LossyCopy(numbers list[int]) {
    list[int]{_|tail} = numbers
    tail
}

func Multidimensional() list[list[string]] {
    list[list[string]]{{"hotdog"}, {"milk", "coffee"}}
}

type Numbers list[int]

func Echo(numbers Numbers) Numbers { numbers }
func Empty() Numbers { Numbers{} }
func Numbers() Numbers { Numbers{1, 2, 3, 4} }
func Numbers(Numbers{head|tail}) Numbers { Numbers{head|{1|tail}} }
```

This syntax is a bit more verbose, with `list[int]{}` for an empty list vs `[]`
from option 1, for example, but it should be easier to parse and process because
type information is always explicit. The cons syntax avoids repeating the type
when a list is specified as the tail, since the type is explicit in the cons
expression itself. Similarly, for multidimensional lists, we don't need to
duplicate type information for list elements, since the outer type communicates
that information..

## Decision outcome

Chosen option: option 2, because it will simplify compiler implementation, and
because making the list type explicit should make code easier to read because
there will be less guessing about what the type is.
