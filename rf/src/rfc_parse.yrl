Nonterminals declaration expression function root type.

Terminals '(' ')' '{' '}' func identifier import package int int_lit string_lit.

Rootsymbol root.

root -> declaration : ['$1'].
root -> declaration root : ['$1'] ++ '$2'.

declaration -> import string_lit : {import, token_line('$2'), token_chars('$2')}.
declaration -> package identifier : {package, token_line('$2'), token_chars('$2')}.
declaration -> function : '$1'.

function -> func identifier '(' ')' type '{' expression '}' : {func, token_line('$1'), token_chars('$2'), [], token_type('$5'), '$7'}.

type -> int : {int, token_line('$1')}.

expression -> int_lit : [{expr, token_line('$1'), {int, token_chars('$1')}}].

Erlang code.

token_chars({_TokenType, _TokenLine, TokenChars}) ->
    TokenChars.

token_line({_TokenType, TokenLine}) ->
    TokenLine;
token_line({_TokenType, TokenLine, _TokenChars}) ->
    TokenLine.

token_type({TokenType, _TokenLine}) ->
    TokenType.
