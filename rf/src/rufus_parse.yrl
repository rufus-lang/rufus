Nonterminals root decl expr exprs function type arg args binary_op.

Terminals '(' ')' '{' '}' ',' '+' '-' '*' '/' func identifier import module bool bool_lit float float_lit int int_lit string string_lit.

Rootsymbol root.

Left 100 '+'.
Left 100 '-'.
Left 100 '*'.
Left 100 '/'.

root -> decl :
    ['$1'].
root -> decl root :
    ['$1'] ++ '$2'.

%% Module-level declarations

decl -> module identifier : rufus_form:make_module(list_to_atom(token_chars('$2')), token_line('$2')).
decl -> import string_lit : rufus_form:make_import(token_chars('$2'), token_line('$2')).
decl -> function          : '$1'.

%% Scalar types

type -> bool   : rufus_form:make_type(bool, token_line('$1')).
type -> float  : rufus_form:make_type(float, token_line('$1')).
type -> int    : rufus_form:make_type(int, token_line('$1')).
type -> string : rufus_form:make_type(string, token_line('$1')).

%% Expressions

exprs -> expr exprs : ['$1'|'$2'].
exprs -> '$empty'   : [].
expr  -> bool_lit   : rufus_form:make_literal(bool, token_chars('$1'), token_line('$1')).
expr  -> float_lit  : rufus_form:make_literal(float, token_chars('$1'), token_line('$1')).
expr  -> int_lit    : rufus_form:make_literal(int, token_chars('$1'), token_line('$1')).
expr  -> string_lit : rufus_form:make_literal(string, list_to_binary(token_chars('$1')), token_line('$1')).
expr  -> identifier : rufus_form:make_identifier(list_to_atom(token_chars('$1')), token_line('$1')).
expr  -> binary_op  : '$1'.

%% Binary operations

binary_op -> expr '+' expr : rufus_form:make_binary_op('+', '$1', '$3', token_line('$2')).
binary_op -> expr '-' expr : rufus_form:make_binary_op('-', '$1', '$3', token_line('$2')).
binary_op -> expr '*' expr : rufus_form:make_binary_op('*', '$1', '$3', token_line('$2')).
binary_op -> expr '/' expr : rufus_form:make_binary_op('/', '$1', '$3', token_line('$2')).

%% Function declarations

function -> func identifier '(' args ')' type '{' exprs '}' : rufus_form:make_func(list_to_atom(token_chars('$2')), '$4', '$6', '$8', token_line('$1')).

args -> arg args            : ['$1'|'$2'].
args -> '$empty'            : [].
arg  -> identifier type ',' : rufus_form:make_arg(list_to_atom(token_chars('$1')), '$2', token_line('$1')).
arg  -> identifier type     : rufus_form:make_arg(list_to_atom(token_chars('$1')), '$2', token_line('$1')).

Erlang code.

token_chars({_TokenType, _Line, Chars}) ->
    Chars.

token_line({_TokenType, Line}) ->
    Line;
token_line({_TokenType, Line, _Chars}) ->
    Line.
