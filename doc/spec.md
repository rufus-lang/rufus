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

The form a … b represents the set of characters from a through b as
alternatives. The horizontal ellipsis … is also used elsewhere in the spec to
informally denote various enumerations or code snippets that are not further
specified. The character … (as opposed to the three characters ...) is not a
token of the Rufus language.

## Source code representation

Source code is Unicode text encoded in UTF-8. The text is not canonicalized, so
a single accented code point is distinct from the same character constructed
from combining an accent and a letter; those are treated as two code points. For
simplicity, this document will use the unqualified term character to refer to a
Unicode code point in the source text.

Each code point is distinct; for instance, upper and lower case letters are
different characters.

Implementation restriction: For compatibility with other tools, a compiler may
disallow the NUL character (U+0000) in the source text.

Implementation restriction: For compatibility with other tools, a compiler may
ignore a UTF-8-encoded byte order mark (U+FEFF) if it is the first Unicode code
point in the source text. A byte order mark may be disallowed anywhere else in
the source.

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
categories Lu, Ll, Lt, Lm, or Lo as Unicode letters, and those in the Number
category Nd as Unicode digits.

### Letters and digits

The underscore character _ (U+005F) is considered a letter.

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
unvigesimal_digit        = "0" … "9" | "A" … "K" | "a" … "k" .
duovigesimal_digit       = "0" … "9" | "A" … "L" | "a" … "l" .
trivigesimal_digit       = "0" … "9" | "A" … "M" | "a" … "m" .
quattuorvigesimal_digit  = "0" … "9" | "A" … "N" | "a" … "n" .
quinvigesimal_digit      = "0" … "9" | "A" … "O" | "a" … "o" .
sexavigesimal_digit      = "0" … "9" | "A" … "P" | "a" … "p" .
septevigesimal_digit     = "0" … "9" | "A" … "Q" | "a" … "q" .
octovigesimal_digit      = "0" … "9" | "A" … "R" | "a" … "r" .
nonavigesimal_digit      = "0" … "9" | "A" … "S" | "a" … "s" .
trigesimal_digit         = "0" … "9" | "A" … "T" | "a" … "t" .
untrigesimal_digit       = "0" … "9" | "A" … "U" | "a" … "u" .
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
formed from spaces (U+0020) and horizontal tabs (U+0009) is ignored except as it
separates tokens that would otherwise combine into a single token. While
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
base 10. An optional prefix can be used to specify the base in the range 2…36.
In integer literals with a base greater than 10, letters starting at `a` and
increasing monotonically from there represent values greater than 9.

```
int_lit                 = binary_lit |
                          ternary_lit |
                          quaternary_lit |
                          quinary_lit |
                          senary_lit |
                          septenary_lit |
                          octoary_lit |
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
                          unvigesimal_lit |
                          duovigesimal_lit |
                          trivigesimal_lit |
                          quattuorvigesimal_lit |
                          quinvigesimal_lit |
                          sexvigesimal_lit |
                          septevigesimal_lit |
                          octovigesimal_lit |
                          nonavigesimal_lit |
                          trigesimal_lit |
                          untrigesimal_lit |
                          duotrigesimal_lit |
                          tritrigesimal_lit |
                          quattuortrigresimal_lit |
                          quintrigesimal_lit |
                          sextrigesimal_lit .
binary_lit              = "2#" binary_digit { binary_digit} .
ternary_lit             = "3#" ternary_digit { ternary_digit} .
quaternary_lit          = "4#" quaternary_digit { quaternary_digit} .
quinary_lit             = "5#" quinary_digit { quinary_digit} .
senary_lit              = "6#" senary_digit { senary_digit} .
septenary_lit           = "7#" septenary_digit { septenary_digit} .
octoary_lit             = "8#" octoary_digit { octoary_digit} .
nonary_lit              = "9#" nonary_digit { nonary_digit} .
decimal_lit             = (( decimal_digit ) { decimal_digit }) | ("10#" ( decimal_digit ) { decimal_digit }) .
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
unvigesimal_lit         = "21#" unvigesimal_digit { unvigesimal_digit} .
duovigesimal_lit        = "22#" duovigesimal_digit { duovigesimal_digit} .
trivigesimal_lit        = "23#" trivigesimal_digit { trivigesimal_digit} .
quattuorvigesimal_lit   = "24#" quattuorvigesimal_digit { quattuorvigesimal_digit} .
quinvigesimal_lit       = "25#" quinvigesimal_digit { quinvigesimal_digit} .
sexvigesimal_lit        = "26#" sexvigesimal_digit { sexvigesimal_digit} .
septevigesimal_lit      = "27#" septevigesimal_digit { septevigesimal_digit} .
octovigesimal_lit       = "28#" octovigesimal_digit { octovigesimal_digit} .
nonavigesimal_lit       = "29#" nonavigesimal_digit { nonavigesimal_digit} .
trigesimal_lit          = "30#" trigesimal_digit { trigesimal_digit} .
untrigesimal_lit        = "31#" untrigesimal_digit { untrigesimal_digit} .
duotrigesimal_lit       = "32#" duotrigesimal_digit { duotrigesimal_digit} .
tritrigesimal_lit       = "33#" tritrigesimal_digit { tritrigesimal_digit} .
quattuortrigresimal_lit = "34#" quattuortrigresimal_digit { quattuortrigresimal_digit} .
quintrigesimal_lit      = "35#" quintrigesimal_digit { quintrigesimal_digit} .
sextrigesimal_lit       = "36#" sextrigesimal_digit { sextrigesimal_digit} .
```
```
000003
42
8#60
29#TheRufusProgrammingLanguage
170141183460469231731687303715884105727
```
