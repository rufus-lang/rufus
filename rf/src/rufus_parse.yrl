Nonterminals declaration expression function root type.

Terminals '(' ')' '{' '}' func identifier import package bool bool_lit float float_lit int int_lit string string_lit.

Rootsymbol root.

root -> declaration : ['$1'].
root -> declaration root : ['$1'] ++ '$2'.

declaration -> import string_lit : {import, token_line('$2'), token_chars('$2')}.
declaration -> package identifier : {package, token_line('$2'), token_chars('$2')}.
declaration -> function : '$1'.

function -> func identifier '(' ')' type '{' expression '}' : {func, token_line('$1'), token_chars('$2'), [], token_type('$5'), '$7'}.

type -> bool : {bool, token_line('$1')}.
type -> float : {float, token_line('$1')}.
type -> int : {int, token_line('$1')}.
type -> string : {string, token_line('$1')}.

expression -> bool_lit : [{expr, token_line('$1'), {bool, token_chars('$1')}}].
expression -> float_lit : [{expr, token_line('$1'), {float, token_chars('$1')}}].
expression -> int_lit : [{expr, token_line('$1'), {int, token_chars('$1')}}].
expression -> string_lit : [{expr, token_line('$1'), {string, token_chars('$1')}}].

Erlang code.

token_chars({_TokenType, _TokenLine, TokenChars}) ->
    TokenChars.

token_line({_TokenType, TokenLine}) ->
    TokenLine;
token_line({_TokenType, TokenLine, _TokenChars}) ->
    TokenLine.

token_type({TokenType, _TokenLine}) ->
    TokenType.
