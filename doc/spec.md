# The Rufus programming language specification

## Introduction

Rufus is a general-purpose programming language for the BEAM. Erlang and Go are
primary influences in the design and ergonomics of the language. It is designed
with teams building and operating large systems over long periods of time in
mind.

This specification is heavily influenced by [The Go Programming Language
Specification](https://golang.org/ref/spec#Introduction). Several sections have been reproduced without changes and
many other sections are modifications based on work created and shared by Google
and used according to terms described in the [Creative Commons 3.0 Attribution
License](https://creativecommons.org/licenses/by/3.0/).

This specification is licensed under the [Creative Commons 3.0 Attribution
License](https://creativecommons.org/licenses/by/3.0/).

## Table of contents

* [Notation](#notation)
* [Source code representation](#source-code-representation)
  * [Characters](#characters)
  * [Letters and digits](#letters-and-digits)
* [Lexical elements](#lexical-elements)
  * [Comments](#comments)
  * [Tokens](#tokens)
  * [Semicolons](#semicolons)
  * [Identifiers](#identifiers)
  * [Keywords](#keywords)
  * [Operators and punctuation](#operators-and-punctuation)
  * [Integer literals](#integer-literals)
  * [Floating-point literals](#floating-point-literals)
  * [String literals](#string-literals)
* [Types](#types)
  * [List types](#list-types)
  * [Function types](#function-types)
* [Blocks](#blocks)
* [Declarations and scope](#declarations-and-scope)
  * [Blank identifier](#blank-identifier)
  * [Predeclared identifiers](#predeclared-identifiers)
  * [Exported identifiers](#exported-identifiers)
* [Expressions](#expressions)
  * [Operands](#operands)
  * [Qualified operators](#qualified-operators)
  * [Function literals](#function-literals)
* [Modules](#modules)
  * [Source file organization](#source-file-organization)
  * [Module clause](#module-clause)
  * [Import declarations](#import-declarations)
  * [An example module](#an-example-module)

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
as it separates tokens that would otherwise combine into a single token. Also, a
newline or end of file may trigger the insertion of a [semicolon](#semicolon). While
breaking the input into tokens, the next token is the longest sequence of
characters that form a valid token.

### Semicolons

The formal grammar uses semicolons `;` as terminators in a number of
productions. Rufus programs may omit most of these semicolons using the
following two rules:

1. When the input is broken into tokens, a semicolon is automatically inserted
   into the token stream immediately after a line's final token if that token
   is:
   - An identifier.
   - A `bool`, `int`, `float` or `string` literal.
   - One of the punctuation `)` or `}`.
2. To allow complex statements to occupy a single line, a semicolon may be
   omitted before a closing `)` or `}`.

To reflect idiomatic use, code examples in this document elide semicolons using
these rules.

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
ぁIsExported
αβ
```

Some identifiers are predeclared.

### Keywords

The following keywords are reserved and may not be used as identifiers.

```
func
module
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
ListType = "list[" ElementType "]" .
```
```
list[string]
list[list[int]]
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

## Blocks

A *block* is a possibly empty sequence of declarations and statements within
matching brace brackets.

```
Block = "{" StatementList "}" .
StatementList = { Statement ";" } .
```

In addition to explicit blocks in the source code, there are implicit blocks:

1. The universe block encompasses all Rufus source text.
2. Each module has a *module block* containing all Rufus source text for that
   module.
3. Each file has a *file block* containing all Rufus source text in that file.
4. Each "if", "for", and "match" statement is considered to be in its own
   implicit block.
5. Each clause in a "match" statement acts as an implicit block.

Blocks nest and influence scoping.

## Declarations and scope

A *declaration* binds a non-blank identifier to a constant, type, variable,
function, label, or module. Every identifier in a program must be declared. No
identifier may be declared twice in the same block, and no identifier may be
declared in both the file and module block.

The blank identifier may be used like any other identifier in a declaration, but
it does not introduce a binding and thus is not declared.

```
Declaration   = ConstDecl | TypeDecl .
TopLevelDecl  = Declaration | FunctionDecl .
```

The scope of a declared identifier is the extent of source text in which the
identifier denotes the specified constant, type, variable, function, or module.

Rufus is lexically scoped using blocks:

1. The scope of a predeclared identifier is the universe block.
2. The scope of an identifier denoting a constant, type, variable, or function
   declared at top level (outside any function) is the module block.
3. The scope of the module name of an imported module is the file block of the
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

The module clause is not a declaration; the module name does not appear in any
scope. Its purpose is to identify the files belonging to the same module and to
specify the default module name for import declarations.

### Blank identifier

The *blank identifier* is represented by the underscore character `_`. It serves
as an anonymous placeholder instead of a regular (non-blank) identifier and has
special meaning in declarations, as an operand, and in assignments.

### Predeclared identifiers

The following identifiers are implicitly declared in the universe block:

```
Types:
	bool float int string

Constants:
	true false
```

### Exported identifiers

An identifier may be *exported* to permit access to it from another module. An
identifier is not exported if either:

1. the first character of the identifier's name starts with an `_`;
2. the first character of the identifier's name starts with a Unicode lower case
   letter (Unicode class "Ll").

All other identifiers are exported.

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

A qualified identifier is an identifier qualified with a module name prefix.
Both the module name and the identifier must not be blank.

```
QualifiedIdent = ModuleName "." identifier .
```

A qualified identifier accesses an identifier in a different module, which must
be imported. The identifier must be exported and declared in the module block of
that module.

```
math.Sin // denotes the Sin function in module math
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

## Modules

Rufus programs are constructed by linking together modules. A module in turn is
constructed from one or more source files that together declare constants,
types, and functions belonging to the module and which are accessible in all
files of the same module. Those elements may be exported and used in another
module.

### Source file organization

Each source file consists of a module clause defining the module to which it
belongs, followed by a possibly empty set of import declarations that declare
modules whose contents it wishes to use, followed by a possibly empty set of
declarations of constants, types, and functions.

```
SourceFile       = ModuleClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
```

### Module clause

A module clause begins each source file and defines the module to which the
file belongs.

```
ModuleClause  = "module" ModuleName .
ModuleName    = identifier .
```

The ModuleName must not be the [blank identifier](#blank-identifier).

```
module math
```

A set of files sharing the same ModuleName form the implementation of a
module. An implementation may require that all source files for a module
inhabit the same directory.

### Import declarations

An import declaration states that the source file containing the declaration
depends on functionality of the _imported_ module and enables access to
exported identifiers of that module. The import names an identifier
(ModuleName) to be used for access and an ImportPath that specifies the module
to be imported.

```
ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
ImportSpec       = [ "." | ModuleName ] ImportPath .
ImportPath       = string_lit .
```

The ModuleName is used in qualified identifiers to access exported identifiers
of the module within the importing source file. It is declared in the file
block. If the ModuleName is omitted, it defaults to the identifier specified in
the module clause of the imported module. If an explicit period (`.`) appears
instead of a name, all the module's exported identifiers declared in that
module's module block will be declared in the importing source file's file block
and must be accessed without a qualifier.

The interpretation of the ImportPath is implementation-dependent but it is
typically a substring of the full file name of the compiled module and may be
relative to a repository of installed modules.

Implementation restriction: A compiler may restrict ImportPaths to non-empty
strings using only characters belonging to Unicode's L, M, N, P, and S general
categories (the Graphic characters without spaces) and may also exclude the
characters `!"#$%&'()*,:;<=>?[\]^`{|}` and the Unicode replacement character
`U+FFFD`.

Assume we have compiled a module containing the module clause module math, which
exports function `Sin`, and installed the compiled module in the file identified
by "`lib/math`". This table illustrates how `Sin` is accessed in files that
import the module after the various types of import declaration.

```
Import declaration          Local name of Sin

import   "lib/math"         math.Sin
import m "lib/math"         m.Sin
```

An import declaration declares a dependency relation between the importing and
imported module. It is illegal for a module to import itself, directly or
indirectly, or to directly import a module without referring to any of its
exported identifiers.

### An example module

Here is a complete Rufus module that implements a prime sieve.

```
module main

import "std/List"

sieve([] list[int]) list[int] {
    []
}
sieve([h|t] list[int]) list[int] {
    result = List.Filter(func(n int) bool { n % h != 0 }, t)
    [h|sieve(result)]
}

main() {
    sieve(List.Seq(2, 30))
}
```
