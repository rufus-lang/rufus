Nonterminals root.

Terminals package identifier.

Rootsymbol root.

root -> package identifier : {package, unwrap('$2')}.

Erlang code.

unwrap({_TokenType, _Line, Symbol}) ->
    Symbol.
