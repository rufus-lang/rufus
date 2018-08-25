Definitions.

Newline       = \n
UnicodeLetter = [A-Za-z]
Digit         = [0-9]
Letter        = ({UnicodeLetter}|"_")
Identifier    = {Letter}({Letter}|{Digit})*
Whitespace    = [\s\t]

LeftParen     = \(
RightParen    = \)
LeftBrace     = \{
RightBrace    = \}

ConstType     = const
FloatType     = float
IntType       = int
StringType    = string

Package       = package
Import        = import
Func          = func

Exponent      = (e|E)?(\+|\-)?{Digit}+
FloatLiteral  = \-?{Digit}+\.{Digit}+{Exponent}?
IntLiteral    = \-?{Digit}+
StringLiteral = \"({UnicodeLetter}|{Whitespace})+\"

Comma         = ,
Match         = =
Plus          = \+

Rules.

{Whitespace}+   : skip_token.
{Newline}+      : skip_token.

{ConstType}     : {token, {const, TokenLine}}.
{FloatType}     : {token, {float, TokenLine}}.
{IntType}       : {token, {int, TokenLine}}.
{StringType}    : {token, {string, TokenLine}}.

{Package}       : {token, {package, TokenLine}}.
{Import}        : {token, {import, TokenLine}}.
{Func}          : {token, {func, TokenLine}}.

{FloatLiteral}  : {token, {float_lit, TokenLine, list_to_float(TokenChars)}}.
{IntLiteral}    : {token, {int_lit, TokenLine, list_to_integer(TokenChars)}}.
{StringLiteral} : S = strip(TokenChars, TokenLen),
                  {token, {string_lit, TokenLine, S}}.

{LeftParen}     : {token, {'(', TokenLine}}.
{RightParen}    : {token, {')', TokenLine}}.
{LeftBrace}     : {token, {'{', TokenLine}}.
{RightBrace}    : {token, {'}', TokenLine}}.
{Comma}         : {token, {',', TokenLine}}.
{Match}         : {token, {'=', TokenLine}}.
{Plus}          : {token, {'+', TokenLine}}.
{Identifier}    : {token, {identifier, TokenLine, TokenChars}}.

Erlang code.

strip(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).
