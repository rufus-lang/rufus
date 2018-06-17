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
letter                   = unicode_letter | "_" .
binary_digit             = "0" … "1" .
ternary_digit            = "0" … "2" .
quaternary_digit         = "0" … "3" .
quinary_digit            = "0" … "4" .
senary_digit             = "0" … "5" .
septenary_digit          = "0" … "6" .
octonary_digit           = "0" … "7" .
nonary_digit             = "0" … "8" .
decimal_digit            = "0" … "9" .
undenary_digit           = "0" … "9" | "A" | "a" .
duodecimal_digit         = "0" … "9" | "A" … "B" | "a" … "b" .
tridecimal_digit         = "0" … "9" | "A" … "C" | "a" … "c" .
quattuordecimal_digit    = "0" … "9" | "A" … "D" | "a" … "d" .
quindecimal_digit        = "0" … "9" | "A" … "E" | "a" … "e" .
sexadecimal_digit        = "0" … "9" | "A" … "F" | "a" … "f" .
septendecimal_digit      = "0" … "9" | "A" … "G" | "a" … "g" .
octodecimal_digit        = "0" … "9" | "A" … "H" | "a" … "h" .
nonadecimal_digit        = "0" … "9" | "A" … "I" | "a" … "i" .
vigesimal_digit          = "0" … "9" | "A" … "J" | "a" … "j" .
unvigenary_digit         = "0" … "9" | "A" … "K" | "a" … "k" .
duovigesimal_digit       = "0" … "9" | "A" … "L" | "a" … "l" .
trivigesimal_digit       = "0" … "9" | "A" … "M" | "a" … "m" .
quattuorvigesimal_digit  = "0" … "9" | "A" … "N" | "a" … "n" .
quinvigesimal_digit      = "0" … "9" | "A" … "O" | "a" … "o" .
sexavigesimal_digit      = "0" … "9" | "A" … "P" | "a" … "p" .
septevigesimal_digit     = "0" … "9" | "A" … "Q" | "a" … "q" .
octovigesimal_digit      = "0" … "9" | "A" … "R" | "a" … "r" .
nonavigesimal_digit      = "0" … "9" | "A" … "S" | "a" … "s" .
trigesimal_digit         = "0" … "9" | "A" … "T" | "a" … "t" .
untrigenary_digit        = "0" … "9" | "A" … "U" | "a" … "u" .
duotrigesimal_digit      = "0" … "9" | "A" … "V" | "a" … "v" .
tritrigesimal_digit      = "0" … "9" | "A" … "W" | "a" … "w" .
quattuortrigesimal_digit = "0" … "9" | "A" … "X" | "a" … "x" .
quintrigesimal_digit     = "0" … "9" | "A" … "Y" | "a" … "y" .
sexatrigesimal_digit     = "0" … "9" | "A" … "Z" | "a" … "z" .
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

An integer literal is a sequence of digits representing an integer constant in
base 10. An optional prefix can be used to specify the base in the range 2
through 36. In integer literals with a base greater than 10, letters starting at
`a` and increasing monotonically from there represent values greater than 9.

```
int_lit                 = binary_lit |
                          ternary_lit |
                          quaternary_lit |
                          quinary_lit |
                          senary_lit |
                          septenary_lit |
                          octonary_lit |
                          nonary_lit |
                          decimal_lit |
                          undenary_lit |
                          duodecimal_lit |
                          tridecimal_lit |
                          quattuordecimal_lit |
                          quindecimal_lit |
                          sexadecimal_lit |
                          septedecimal_lit |
                          octodecimal_lit |
                          nonadecimal_lit |
                          vigesimal_lit |
                          unvigenary_lit |
                          duovigesimal_lit |
                          trivigesimal_lit |
                          quattuorvigesimal_lit |
                          quinvigesimal_lit |
                          sexavigesimal_lit |
                          septevigesimal_lit |
                          octovigesimal_lit |
                          nonavigesimal_lit |
                          trigesimal_lit |
                          untrigenary_lit |
                          duotrigesimal_lit |
                          tritrigesimal_lit |
                          quattuortrigresimal_lit |
                          quintrigesimal_lit |
                          sexatrigesimal_lit .
binary_lit              = "2#" binary_digit { binary_digit} .
ternary_lit             = "3#" ternary_digit { ternary_digit} .
quaternary_lit          = "4#" quaternary_digit { quaternary_digit} .
quinary_lit             = "5#" quinary_digit { quinary_digit} .
senary_lit              = "6#" senary_digit { senary_digit} .
septenary_lit           = "7#" septenary_digit { septenary_digit} .
octonary_lit            = "8#" octonary_digit { octonary_digit} .
nonary_lit              = "9#" nonary_digit { nonary_digit} .
decimal_lit             = "10#" decimal_digit { decimal_digit } |
                           decimal_digit { decimal_digit } .
undenary_lit            = "11#" undenary_digit { undenary_digit} .
duodecimal_lit          = "12#" duodecimal_digit { duodecimal_digit} .
tridecimal_lit          = "13#" tridecimal_digit { tridecimal_digit} .
quattuordecimal_lit     = "14#" quattuordecimal_digit { quattuordecimal_digit} .
quindecimal_lit         = "15#" quindecimal_digit { quindecimal_digit} .
sexadecimal_lit         = "16#" sexadecimal_digit { sexadecimal_digit} .
septedecimal_lit        = "17#" septedecimal_digit { septedecimal_digit} .
octodecimal_lit         = "18#" octodecimal_digit { octodecimal_digit} .
nonadecimal_lit         = "19#" nonadecimal_digit { nonadecimal_digit} .
vigesimal_lit           = "20#" vigesimal_digit { vigesimal_digit} .
unvigenary_lit          = "21#" unvigenary_digit { unvigenary_digit} .
duovigesimal_lit        = "22#" duovigesimal_digit { duovigesimal_digit} .
trivigesimal_lit        = "23#" trivigesimal_digit { trivigesimal_digit} .
quattuorvigesimal_lit   = "24#" quattuorvigesimal_digit { quattuorvigesimal_digit} .
quinvigesimal_lit       = "25#" quinvigesimal_digit { quinvigesimal_digit} .
sexavigesimal_lit       = "26#" sexavigesimal_digit { sexavigesimal_digit} .
septevigesimal_lit      = "27#" septevigesimal_digit { septevigesimal_digit} .
octovigesimal_lit       = "28#" octovigesimal_digit { octovigesimal_digit} .
nonavigesimal_lit       = "29#" nonavigesimal_digit { nonavigesimal_digit} .
trigesimal_lit          = "30#" trigesimal_digit { trigesimal_digit} .
untrigenary_lit         = "31#" untrigenary_digit { untrigenary_digit} .
duotrigesimal_lit       = "32#" duotrigesimal_digit { duotrigesimal_digit} .
tritrigesimal_lit       = "33#" tritrigesimal_digit { tritrigesimal_digit} .
quattuortrigresimal_lit = "34#" quattuortrigresimal_digit { quattuortrigresimal_digit} .
quintrigesimal_lit      = "35#" quintrigesimal_digit { quintrigesimal_digit} .
sexatrigesimal_lit      = "36#" sexatrigesimal_digit { sexatrigesimal_digit} .
```
```
000003
42
8#60
31#TheRufusProgrammingLanguage
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
