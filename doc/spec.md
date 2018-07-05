# The Rufus programming language specification

## Introduction

Rufus is a general-purpose programming language for the BEAM. Erlang and Go are
primary influences in the design and ergonomics of the language. It is designed
for many people to effectively build large systems over long periods of time.

This specification is heavily influenced by [The Go Programming Language
Specification](https://golang.org/ref/spec#Introduction). Many sections have been reproduced without changes and many
other sections are modifications based on work created and shared by Google and
used according to terms described in the [Creative Commons 3.0 Attribution
License](https://creativecommons.org/licenses/by/3.0/).

This specification is licensed under the [Creative Commons 3.0 Attribution
License](https://creativecommons.org/licenses/by/3.0/).

## Notation

The syntax is specified using Extended Backus-Naur Form (EBNF):

```
Production  = production_name "=" [ Expression ] "." .
Expression  = Alternative { "|" Alternative } .
Alternative = Term { Term } .
Term        = production_name | token [ "…" token ] | Group | Option | Repetition .
Group       = "(" Expression ")" .
Option      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
```

Productions are expressions constructed from terms and the following operators,
in increasing precedence:

```
|   alternation
()  grouping
[]  option (0 or 1 times)
{}  repetition (0 to n times)
```

Lower-case production names are used to identify lexical tokens. Non-terminals
are in CamelCase. Lexical tokens are enclosed in double quotes "" or back quotes
\`\`.

The form `a … b` represents the set of characters from `a` through `b` as
alternatives. The horizontal ellipsis `…` is also used elsewhere in the spec to
informally denote various enumerations or code snippets that are not further
specified. The character `…` is not a token of the Rufus language.

## Source code representation

Source code is Unicode text encoded in UTF-8. The text is not canonicalized, so
a single accented code point is distinct from the same character constructed
from combining an accent and a letter; those are treated as two code points. For
simplicity, this document will use the unqualified term character to refer to a
Unicode code point in the source text.

Each code point is distinct; for instance, upper and lower case letters are
different characters.

Implementation restriction: For compatibility with other tools, a compiler may
disallow the `NUL` character (`U+0000`) in the source text.

Implementation restriction: For compatibility with other tools, a compiler may
ignore a UTF-8-encoded byte order mark (`U+FEFF`) if it is the first Unicode
code point in the source text. A byte order mark may be disallowed anywhere else
in the source.

### Characters

The following terms are used to denote specific Unicode character classes:

```
newline        = /* the Unicode code point U+000A */ .
unicode_char   = /* an arbitrary Unicode code point except newline */ .
unicode_letter = /* a Unicode code point classified as "Letter" */ .
unicode_digit  = /* a Unicode code point classified as "Number, decimal digit" */ .
```

In [The Unicode Standard 8.0](http://www.unicode.org/versions/Unicode8.0.0/), Section 4.5 "General Category" defines a set
of character categories. Rufus treats all characters in any of the Letter
categories `Lu`, `Ll`, `Lt`, `Lm`, or `Lo` as Unicode letters, and those in the
Number category `Nd` as Unicode digits.

### Letters and digits

The underscore character `_` (`U+005F`) is considered a letter.

```
letter        = unicode_letter | "_" .
decimal_digit = "0" … "9" .
octal_digit   = "0" … "7" .
hex_digit     = "0" … "9" | "A" … "F" | "a" … "f" .```
```

## Lexical elements

### Comments

Comments start with the character sequence `//` and stop at the end of the line.

### Tokens

Tokens form the vocabulary of the Rufus language. There are four classes:
identifiers, keywords, operators and punctuation, and literals. White space,
formed from spaces (`U+0020`) and horizontal tabs (`U+0009`) is ignored except
as it separates tokens that would otherwise combine into a single token. While
breaking the input into tokens, the next token is the longest sequence of
characters that form a valid token.

### Identifiers

Identifiers name program entities such as variables and types. An identifier is
a sequence of one or more letters and digits. The first character in an
identifier must be a letter.

```
identifier = letter { letter | unicode_digit } .
```
```
a
_x9
ThisVariableIsExported
αβ
```

Some identifiers are predeclared.

### Keywords

The following keywords are reserved and may not be used as identifiers.

```
break        default      func         interface    select
case         defer        go           map          struct
chan         else         goto         package      switch
const        fallthrough  if           range        type
continue     for          import       return       var
```

### Operators and punctuation

The following character sequences represent operators (including assignment
operators) and punctuation:

```
+    &     +=    &=     &&    ==    !=    (    )
-    |     -=    |=     ||    <     <=    [    ]
*    ^     *=    ^=     <-    >     >=    {    }
/    <<    /=    <<=    ++    =     :=    ,    ;
%    >>    %=    >>=    --    !     ...   .    :
     &^          &^=
```

### Integer literals

An integer literal is a sequence of digits representing an integer constant. An
optional prefix sets a non-decimal base: `0` for octal, `0x` or `0X` for
hexadecimal. In hexadecimal literals, letters `a-f` and `A-F` represent values
10 through 15.

```
int_lit     = decimal_lit | octal_lit | hex_lit .
decimal_lit = ( "1" … "9" ) { decimal_digit } .
octal_lit   = "0" { octal_digit } .
hex_lit     = "0" ( "x" | "X" ) hex_digit { hex_digit } .
```
```
42
0600
0xBadFace
170141183460469231731687303715884105727
```

### Floating-point literals

A floating-point literal is a decimal representation of a floating-point
constant. It has an integer part, a decimal point, a fractional part, and an
exponent part. The integer and fractional part comprise decimal digits; the
exponent part is an `e` or `E` followed by an optionally signed decimal
exponent. One of the integer part or the fractional part may be elided; one of
the decimal point or the exponent may be elided.

```
float_lit = decimals "." [ decimals ] [ exponent ] |
            decimals exponent |
            "." decimals [ exponent ] .
decimals  = decimal_digit { decimal_digit } .
exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals .
```
```
0.
72.40
072.40
2.71828
1.e+0
6.67428e-11
1E6
.25
.12345E+5
```

### Rune literals

A rune literal represents a rune constant, an integer value identifying a
Unicode code point. A rune literal is expressed as one or more characters
enclosed in single quotes, as in `'x'` or `'\n'`. Within the quotes, any
character may appear except newline and unescaped single quote. A single quoted
character represents the Unicode value of the character itself, while
multi-character sequences beginning with a backslash encode values in various
formats.

The simplest form represents the single character within the quotes; since Rufus
source text is Unicode characters encoded in UTF-8, multiple UTF-8-encoded bytes
may represent a single integer value. For instance, the literal `'a'` holds a
single byte representing a literal `a`, Unicode `U+0061`, value `0x61`, while
`'ä'` holds two bytes (`0xc3` `0xa4`) representing a literal `a`-dieresis,
`U+00E4`, value `0xe4`.

Several backslash escapes allow arbitrary values to be encoded as ASCII text.
There are four ways to represent the integer value as a numeric constant: `\x`
followed by exactly two hexadecimal digits; `\u` followed by exactly four
hexadecimal digits; `\U` followed by exactly eight hexadecimal digits, and a
plain backslash `\` followed by exactly three octal digits. In each case the
value of the literal is the value represented by the digits in the corresponding
base.

Although these representations all result in an integer, they have different
valid ranges. Octal escapes must represent a value between `0` and `255`
inclusive. Hexadecimal escapes satisfy this condition by construction. The
escapes `\u` and `\U` represent Unicode code points so within them some values
are illegal, in particular those above `0x10FFFF` and surrogate halves.

After a backslash, certain single-character escapes represent special values:

```
\a   U+0007 alert or bell
\b   U+0008 backspace
\f   U+000C form feed
\n   U+000A line feed or newline
\r   U+000D carriage return
\t   U+0009 horizontal tab
\v   U+000b vertical tab
\\   U+005c backslash
\'   U+0027 single quote  (valid escape only within rune literals)
\"   U+0022 double quote  (valid escape only within string literals)
```

All other sequences starting with a backslash are illegal inside rune literals.

```
rune_lit         = "'" ( unicode_value | byte_value ) "'" .
unicode_value    = unicode_char | little_u_value | big_u_value | escaped_char .
byte_value       = octal_byte_value | hex_byte_value .
octal_byte_value = `\` octal_digit octal_digit octal_digit .
hex_byte_value   = `\` "x" hex_digit hex_digit .
little_u_value   = `\` "u" hex_digit hex_digit hex_digit hex_digit .
big_u_value      = `\` "U" hex_digit hex_digit hex_digit hex_digit
                           hex_digit hex_digit hex_digit hex_digit .
escaped_char     = `\` ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | `\` | "'" | `"` ) .
```
```
'a'
'ä'
'本'
'\t'
'\000'
'\007'
'\377'
'\x07'
'\xff'
'\u12e4'
'\U00101234'
'\''         // rune literal containing single quote character
'aa'         // illegal: too many characters
'\xa'        // illegal: too few hexadecimal digits
'\0'         // illegal: too few octal digits
'\uDFFF'     // illegal: surrogate half
'\U00110000' // illegal: invalid Unicode code point
```

### String literals

A string literal represents a string constant obtained from concatenating a
sequence of characters. String literals are character sequences between double
quotes, as in `"bar"`. Within the quotes, any character may appear except
newline and unescaped double quote. The text between the quotes forms the value
of the literal, with backslash escapes interpreted as they are in rune literals
(except that `\'` is illegal and `\"` is legal), with the same restrictions. The
three-digit octal (`\nnn`) and two-digit hexadecimal (`\xnn`) escapes represent
individual bytes of the resulting string; all other escapes represent the
(possibly multi-byte) UTF-8 encoding of individual characters. Thus inside a
string literal `\377` and `\xFF` represent a single byte of value `0xFF=255`,
while `ÿ`, `\u00FF`, `\U000000FF` and `\xc3\xbf` represent the two bytes `0xc3
0xbf` of the UTF-8 encoding of character `U+00FF`.

```
string_lit = `"` { unicode_value | byte_value } `"` .
```
```
"\n"
"\""
"Hello, world!\n"
"日本語"
"\u65e5本\U00008a9e"
"\xff\u00FF"
"\uD800"             // illegal: surrogate half
"\U00110000"         // illegal: invalid Unicode code point
```

These examples all represent the same string:

```
"日本語"                                 // UTF-8 input text
"\u65e5\u672c\u8a9e"                    // the explicit Unicode code points
"\U000065e5\U0000672c\U00008a9e"        // the explicit Unicode code points
"\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e"  // the explicit UTF-8 bytes
```

## Types

### List types

A list is an ordered sequence of elements of a single type, called the element
type. A list is either an empty list or consists of a head element and tail
(remainder of the list). The tail is also a list of the same type.

```
ListType = "[" "]" ElementType .
```
```
[]unicode
[][]int
```

Lists are always one-dimensional but may be composed to construct
higher-dimensional objects.

### Function types

A function type denotes the set of all functions with the same parameter and
result types.

```
FunctionType   = "func" Signature .
Signature      = Parameters [ Result ] .
Result         = Type .
Parameters     = "(" [ ParameterList [ "," ] ] ")" .
ParameterList  = ParameterDecl { "," ParameterDecl } .
ParameterDecl  = [ IdentifierList ] Type .
```

Within a list of parameters or results, the names (`IdentifierList`) must either
all be present or all be absent. If present, each name stands for one item
(parameter or result) of the specified type and all non-blank names in the
signature must be unique. If absent, each type stands for one item of that type.
Parameter and result lists are always parenthesized except that if there is
exactly one unnamed result it may be written as an unparenthesized type.

The final incoming parameter in a function signature may have a type prefixed
with `...`. A function with such a parameter is called variadic and may be
invoked with zero or more arguments for that parameter.

```
func()
func(x int) int
func(a, _ int, z float) bool
func(a, b int, z float) (bool)
func(prefix unicode)
func(int, int, float) (float, []int)
func(n int) func(p T)
```

## Declarations and scope

A *declaration* binds a non-blank identifier to a constant, type, variable,
function, label, or package. Every identifier in a program must be declared. No
identifier may be declared twice in the same block, and no identifier may be
declared in both the file and package block.

The blank identifier may be used like any other identifier in a declaration, but
it does not introduce a binding and thus is not declared.

```
Declaration   = ConstDecl | TypeDecl .
TopLevelDecl  = Declaration | FunctionDecl .
```

The scope of a declared identifier is the extent of source text in which the
identifier denotes the specified constant, type, variable, function, label, or
package.

Rufus is lexically scoped using blocks:

1. The scope of a predeclared identifier is the universe block.
2. The scope of an identifier denoting a constant, type, variable, or function
   declared at top level (outside any function) is the package block.
3. The scope of the package name of an imported package is the file block of the
   file containing the import declaration.
4. The scope of an identifier denoting a function parameter, or result variable
   is the function body.
5. The scope of a constant or variable identifier declared inside a function
   begins at the end of the ConstSpec or VarSpec (ShortVarDecl for short
   variable declarations) and ends at the end of the innermost containing block.
6. The scope of a type identifier declared inside a function begins at the
   identifier in the TypeSpec and ends at the end of the innermost containing
   block.

An identifier declared in a block may be redeclared in an inner block. While the
identifier of the inner declaration is in scope, it denotes the entity declared
by the inner declaration.

The package clause is not a declaration; the package name does not appear in any
scope. Its purpose is to identify the files belonging to the same package and to
specify the default package name for import declarations.

### Blank identifier

The *blank identifier* is represented by the underscore character `_`. It serves
as an anonymous placeholder instead of a regular (non-blank) identifier and has
special meaning in declarations, as an operand, and in assignments.

### Predeclared identifiers

The following identifiers are implicitly declared in the universe block:

```
Types:
	atom bool binary float int rune unicode

Constants:
	true false

Functions:
    len
```

## Expressions

An expression specifies the computation of a value by applying operators and
functions to operands.

### Operands

Operands denote the elementary values in an expression. An operand may be a
literal, a (possibly qualified) non-blank identifier denoting a constant,
variable, or function, or a parenthesized expression.

The blank identifier may appear as an operand only on the left-hand side of an
assignment.

```
Operand     = Literal | OperandName | "(" Expression ")" .
Literal     = BasicLit | CompositeLit | FunctionLit .
BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
OperandName = identifier | QualifiedIdent.
```

### Qualified operators

A qualified identifier is an identifier qualified with a package name prefix.
Both the package name and the identifier must not be blank.

```
QualifiedIdent = PackageName "." identifier .
```

A qualified identifier accesses an identifier in a different package, which must
be imported. The identifier must be exported and declared in the package block
of that package.

```
math.Sin // denotes the Sin function in package math
```

### Composite literals

Composite literals construct values for lists, maps, structs, and tuples and
create a new value each time they are evaluated. They consist of the type of the
literal followed by a brace-bound list of elements. Each element may optionally
be preceded by a corresponding key.

```
CompositeLit  = LiteralType LiteralValue .
LiteralType   = ListType | "[" "..." "]" ElementType | MapType | StructType |
                TupleType | TypeName .
LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
ElementList   = KeyedElement { "," KeyedElement } .
KeyedElement  = [ Key ":" ] Element .
Key           = FieldName | Expression | LiteralValue .
FieldName     = identifier .
Element       = Expression | LiteralValue .
```

The LiteralType's underlying type must be a list, map, struct or tuple type (the
grammar enforces this constraint except when the type is given as a TypeName).
The types of the elements and keys must be assignable to the respective field,
element, and key types of the literal type; there is no additional conversion.
The key is interpreted as a field name for struct literals, an index for array
and slice literals, and a key for map literals. For map literals, all elements
must have a key. It is an error to specify multiple elements with the same field
name or constant key value. For non-constant map keys, see the section on
evaluation order.

For struct literals the following rules apply:

- A key must be a field name declared in the struct type.
- An element list must contain a keys for each struct field.
- It is an error to specify an element for a non-exported field of a struct
  belonging to a different package.

Given the declarations

```
type Point struct { x, y float }
type Line struct { p, q Point }
```

one may write

```
origin := Point{x: 1.1, y: -4.2}
line := Line{origin, Point{x: 5.6, y: -0.3}}
```

A list literal describes the entire underlying sequence. A list literal has the
form

```
[]T{x1, x2, … xn}
```

Within a composite literal of list, map, or tuple type T, elements or map keys
that are themselves composite literals may elide the respective literal type if
it is identical to the element or key type of T.

```
[][]int{{1, 2, 3}, {4, 5}}           // same as [][]int{[]int{1, 2, 3}, []int{4, 5}}
[][]Point{{{0, 1}, {1, 2}}}          // same as [][]Point{[]Point{Point{0, 1}, Point{1, 2}}}
map[unicode]Point{"orig": {0, 0}}    // same as map[unicode]Point{"orig": Point{0, 0}}
map[Point]unicode{{0, 0}: "orig"}    // same as map[Point]unicode{Point{0, 0}: "orig"}
tuple[unicode, Point]{"orig", {0, 0} // same as tuple[unicode, Point]{"orig", Point{0,0}}
```

A parsing ambiguity arises when a composite literal using the TypeName form of
the LiteralType appears as an operand between the keyword and the opening brace
of the block of an "if", "for", or "switch" statement, and the composite literal
is not enclosed in parentheses, square brackets, or curly braces. In this rare
case, the opening brace of the literal is erroneously parsed as the one
introducing the block of statements. To resolve the ambiguity, the composite
literal must appear within parentheses.

```
if x == (T{a,b,c}[i]) { … }
if (x == T{a,b,c}[i]) { … }
```

Examples of valid list, map, and tuple literals:

```
// list of prime numbers
primes := []int{2, 3, 5, 7, 9, 2147483647}

// frequencies in Hz for equal-tempered scale (A4 = 440Hz)
noteFrequency := map[string]float{
    "C0": 16.35,"D0": 18.35, "E0": 20.60, "F0": 21.83,
    "G0": 24.50, "A0": 27.50, "B0": 30.87,

// tuple of RGB color codes
purple := tuple[int, int, int]{58, 21, 168}
```

### Function literals

A function literal represents an anonymous function.

```
FunctionLit = "func" Signature FunctionBody .
```
```
func(a, b int, z float) bool { return a*b < int(z) }
```

A function literal can be assigned to a variable or invoked directly.

```
f := func(x, y int) int { x + y }
func(s unicode) { string:reverse(s) }("noon")
```

Function literals are closures: they may refer to variables defined in a
surrounding function. Those variables are then shared between the surrounding
function and the function literal, and they survive as long as they are
accessible.
