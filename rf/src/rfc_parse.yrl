Nonterminals root statement.

Terminals identifier import package string_lit.

Rootsymbol root.

root -> statement : ['$1'].
root -> statement root : ['$1'] ++ '$2'.

statement -> import string_lit : {import, token_line('$2'), token_chars('$2')}.
statement -> package identifier : {package, token_line('$2'), token_chars('$2')}.

Erlang code.

token_chars({_TokenType, _TokenLine, TokenChars}) ->
    TokenChars.

token_line({_TokenType, TokenLine, _TokenChars}) ->
    TokenLine.
