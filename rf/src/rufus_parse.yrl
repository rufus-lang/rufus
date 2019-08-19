%%
%% Token categories
%%

Nonterminals
    root
    decl
    type
    block
    function arg args expr exprs
    binary_op.

Terminals
    '{' '}' '(' ')' ','
    '+' '-' '*' '/' '%'
    ';'
    module import
    func identifier
    atom atom_lit
    bool bool_lit
    float float_lit
    int int_lit
    string string_lit.

%%
%% Root symbol
%%

Rootsymbol root.

%%
%% Operator precedence
%%

Left 100 '+'.
Left 100 '-'.
Left 100 '*'.
Left 100 '/'.
Left 100 '%'.

%%
%% Grammar rules
%%

root -> decl                  : ['$1'].
root -> decl root             : ['$1'] ++ '$2'. %% TODO(jkakar): Optimize this to avoid concatenation.

decl -> module identifier ';' : rufus_form:make_module(list_to_atom(chars('$2')), line('$2')).
decl -> import string_lit ';' : rufus_form:make_import(chars('$2'), line('$2')).
decl -> function              : '$1'.

type -> atom                  : rufus_form:make_type(atom, line('$1')).
type -> bool                  : rufus_form:make_type(bool, line('$1')).
type -> float                 : rufus_form:make_type(float, line('$1')).
type -> int                   : rufus_form:make_type(int, line('$1')).
type -> string                : rufus_form:make_type(string, line('$1')).

function -> func identifier '(' args ')' type block :
                               rufus_form:make_func(list_to_atom(chars('$2')), '$4', '$6', '$7', line('$1')).

args -> arg args              : ['$1'|'$2'].
args -> '$empty'              : [].
arg  -> identifier type ','   : rufus_form:make_arg(list_to_atom(chars('$1')), '$2', line('$1')).
arg  -> identifier type       : rufus_form:make_arg(list_to_atom(chars('$1')), '$2', line('$1')).

block -> '{' exprs '}'        : '$2'.
block -> '{' expr ';' '}'     : ['$2'].

exprs -> expr ';' exprs       : ['$1'|'$3'].
exprs -> '$empty'             : [].
expr  -> atom_lit             : rufus_form:make_literal(atom, chars('$1'), line('$1')).
expr  -> bool_lit             : rufus_form:make_literal(bool, chars('$1'), line('$1')).
expr  -> float_lit            : rufus_form:make_literal(float, chars('$1'), line('$1')).
expr  -> int_lit              : rufus_form:make_literal(int, chars('$1'), line('$1')).
expr  -> string_lit           : rufus_form:make_literal(string, list_to_binary(chars('$1')), line('$1')).
expr  -> identifier           : rufus_form:make_identifier(list_to_atom(chars('$1')), line('$1')).
expr  -> binary_op            : '$1'.

binary_op -> expr '+' expr    : rufus_form:make_binary_op('+', '$1', '$3', line('$2')).
binary_op -> expr '-' expr    : rufus_form:make_binary_op('-', '$1', '$3', line('$2')).
binary_op -> expr '*' expr    : rufus_form:make_binary_op('*', '$1', '$3', line('$2')).
binary_op -> expr '/' expr    : rufus_form:make_binary_op('/', '$1', '$3', line('$2')).
binary_op -> expr '%' expr    : rufus_form:make_binary_op('%', '$1', '$3', line('$2')).

Erlang code.

%% chars returns the content from the token.
chars({_TokenType, _Line, Chars}) ->
    Chars.

%% line returns the line number from the token.
line({_TokenType, Line}) ->
    Line;
line({_TokenType, Line, _Chars}) ->
    Line.
