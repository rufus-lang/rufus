Nonterminals
  root decl expr function type arg args
  .

Terminals '(' ')' '{' '}' ',' func identifier import module bool bool_lit float float_lit int int_lit string string_lit.

Rootsymbol root.

root -> decl : ['$1'].
root -> decl root : ['$1'] ++ '$2'.

decl -> import string_lit : {import, #{line => token_line('$2'), spec => token_chars('$2')}}.
decl -> module identifier : {module, #{line => token_line('$2'), spec => token_atom('$2')}}.
decl -> function : '$1'.

function -> func identifier '(' args ')' type '{' expr '}' : {func, token_line('$1'), token_chars('$2'), '$4', token_type('$6'), '$8'}.

type -> bool : {bool, token_line('$1')}.
type -> float : {float, token_line('$1')}.
type -> int : {int, token_line('$1')}.
type -> string : {string, token_line('$1')}.

args -> arg args : ['$1'|'$2'].
args -> '$empty' : [].
arg -> identifier type ',' : {arg, token_line('$1'), token_atom('$1'), token_type('$2')}.
arg -> identifier type : {arg, token_line('$1'), token_atom('$1'), token_type('$2')}.

expr -> bool_lit : [{expr, token_line('$1'), {bool, token_chars('$1')}}].
expr -> float_lit : [{expr, token_line('$1'), {float, token_chars('$1')}}].
expr -> int_lit : [{expr, token_line('$1'), {int, token_chars('$1')}}].
expr -> string_lit : [{expr, token_line('$1'), {string, token_chars('$1')}}].
expr -> identifier : [{expr, token_line('$1'), {identifier, token_atom('$1')}}].

Erlang code.

token_atom({_TokenType, _TokenLine, TokenChars}) ->
    list_to_atom(TokenChars).
token_chars({_TokenType, _TokenLine, TokenChars}) ->
    TokenChars.

token_line({_TokenType, TokenLine}) ->
    TokenLine;
token_line({_TokenType, TokenLine, _TokenChars}) ->
    TokenLine.

token_type({TokenType, _TokenLine}) ->
    TokenType.
