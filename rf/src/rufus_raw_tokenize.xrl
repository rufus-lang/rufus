Definitions.

Newline       = \n
Colon         = \:
Semicolon     = \;
UnicodeLetter = [A-Za-z]
Digit         = [0-9]
Letter        = ({UnicodeLetter}|'_')
Whitespace    = [\s\t]

Module        = module
Import        = import
Const         = const
Func          = func

AtomType      = atom
BoolType      = bool
FloatType     = float
IntType       = int
StringType    = string

AtomLiteral   = {Colon}({UnicodeLetter}+({Digit}|{UnicodeLetter})?|\'({Digit}|{UnicodeLetter}|{Whitespace})+\')
BoolLiteral   = (true|false)
Exponent      = (e|E)?(\+|\-)?{Digit}+
FloatLiteral  = (\+|\-)?{Digit}+\.{Digit}+{Exponent}?
IntLiteral    = (\+|\-)?{Digit}+
StringLiteral = \"({Digit}|{UnicodeLetter}|{Whitespace})+\"

LeftParen     = \(
RightParen    = \)
LeftBrace     = \{
RightBrace    = \}
Comma         = ,
Match         = =
Plus          = \+
Minus         = \-
Multiply      = \*
Divide        = \/
Remainder     = \%
And           = and
Or            = or
XOr           = xor

Identifier    = {Letter}({Letter}|{Digit})*

Rules.

{Whitespace}+   : skip_token.
{Newline}+      : {token, {eol, TokenLine}}.
{Semicolon}+    : {token, {';', TokenLine}}.

{Module}        : {token, {module, TokenLine}}.
{Import}        : {token, {import, TokenLine}}.
{Const}         : {token, {const, TokenLine}}.
{Func}          : {token, {func, TokenLine}}.

{AtomType}      : {token, {atom, TokenLine}}.
{BoolType}      : {token, {bool, TokenLine}}.
{FloatType}     : {token, {float, TokenLine}}.
{IntType}       : {token, {int, TokenLine}}.
{StringType}    : {token, {string, TokenLine}}.

{AtomLiteral}   : A = trim_atom_markup(TokenChars, TokenLen),
                  {token, {atom_lit, TokenLine, A}}.
{BoolLiteral}   : {token, {bool_lit, TokenLine, list_to_atom(TokenChars)}}.
{FloatLiteral}  : {token, {float_lit, TokenLine, list_to_float(TokenChars)}}.
{IntLiteral}    : {token, {int_lit, TokenLine, list_to_integer(TokenChars)}}.
{StringLiteral} : S = trim_quotes(TokenChars, TokenLen),
                  {token, {string_lit, TokenLine, S}}.

{LeftParen}     : {token, {'(', TokenLine}}.
{RightParen}    : {token, {')', TokenLine}}.
{LeftBrace}     : {token, {'{', TokenLine}}.
{RightBrace}    : {token, {'}', TokenLine}}.
{Comma}         : {token, {',', TokenLine}}.
{Match}         : {token, {'=', TokenLine}}.
{Plus}          : {token, {'+', TokenLine}}.
{Minus}         : {token, {'-', TokenLine}}.
{Multiply}      : {token, {'*', TokenLine}}.
{Divide}        : {token, {'/', TokenLine}}.
{Remainder}     : {token, {'%', TokenLine}}.
{And}           : {token, {'and', TokenLine}}.
{Or}            : {token, {'or', TokenLine}}.
{XOr}           : {token, {'xor', TokenLine}}.

{Identifier}    : {token, {identifier, TokenLine, TokenChars}}.

Erlang code.

%% trim_atom_markup trims the leading colon from an atom. It also trims single
%% quotes when they're present.
trim_atom_markup(TokenChars, TokenLen) ->
    {TokenChars1, TokenLen1} = trim_leading_colon(TokenChars, TokenLen),
    case is_single_quoted(TokenChars1) of
        true ->
            list_to_atom(trim_quotes(TokenChars1, TokenLen1));
        false ->
            list_to_atom(TokenChars1)
    end.

%% trim_leading_colon trims the leading colon from an atom.
trim_leading_colon(TokenChars, TokenLen) ->
    TrimmedTokenLen = TokenLen - 1,
    TrimmedTokenChars = lists:sublist(TokenChars, 2, TrimmedTokenLen),
    {TrimmedTokenChars, TrimmedTokenLen}.

%% trim_quotes trims the quotes around the token.
trim_quotes(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

%% is_single_quoted returns true if the token is wrapped in single quotes,
%% otherwise false.
is_single_quoted(TokenChars) ->
    (hd(TokenChars) == $') and (lists:last(TokenChars) == $').
