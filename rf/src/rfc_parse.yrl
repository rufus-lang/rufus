Nonterminals root import.

Terminals identifier package string_lit.

Rootsymbol root.

root -> package identifier : {package, unwrap('$2')}.
%% import -> string_lit : {import, '$1'}.

Erlang code.

unwrap({_TokenType, _Line, Symbol}) ->
    Symbol.
