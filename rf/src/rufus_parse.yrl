%%
%% Token categories
%%

Nonterminals
    root
    decl
    type
    block
    func_decl param params expr exprs args
    binary_op call cons match.

Terminals
    '[' ']' '{' '}' '(' ')' '|'
    '+' '-' '*' '/' '%'
    ',' ';' '='
    'and' 'or'
    module import
    func identifier
    atom atom_lit
    bool bool_lit
    float float_lit
    int int_lit
    string string_lit
    list.

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
Left 90  'and'.
Left 80  'or'.
Left 50  '='.

%%
%% Grammar rules
%%

root -> decl                     : ['$1'].
root -> decl root                : ['$1'] ++ '$2'.

decl -> module identifier ';'    : rufus_form:make_module(list_to_atom(text('$2')), line('$2')).
decl -> import string_lit ';'    : rufus_form:make_import(text('$2'), line('$2')).
decl -> func_decl                : '$1'.

type -> atom                     : rufus_form:make_type(atom, line('$1')).
type -> bool                     : rufus_form:make_type(bool, line('$1')).
type -> float                    : rufus_form:make_type(float, line('$1')).
type -> int                      : rufus_form:make_type(int, line('$1')).
type -> string                   : rufus_form:make_type(string, line('$1')).
type -> list '[' type ']'        : rufus_form:make_type(list, '$3', line('$1')).

func_decl -> func identifier '(' params ')' type block :
                                   rufus_form:make_func(list_to_atom(text('$2')), '$4', '$6', '$7', line('$1')).

params -> param params           : ['$1'|'$2'].
params -> '$empty'               : [].
param -> identifier type ','     : rufus_form:make_param(list_to_atom(text('$1')), '$2', line('$1')).
param -> identifier type         : rufus_form:make_param(list_to_atom(text('$1')), '$2', line('$1')).

block -> '{' exprs '}' ';'       : '$2'.

args  -> expr ',' args           : ['$1'|'$3'].
args  -> expr                    : ['$1'].
args  -> '$empty'                : [].
exprs -> expr ';' exprs          : ['$1'|'$3'].
exprs -> expr                    : ['$1'].
exprs -> '$empty'                : [].
expr  -> atom_lit                : rufus_form:make_literal(atom, text('$1'), line('$1')).
expr  -> bool_lit                : rufus_form:make_literal(bool, text('$1'), line('$1')).
expr  -> float_lit               : rufus_form:make_literal(float, text('$1'), line('$1')).
expr  -> int_lit                 : rufus_form:make_literal(int, text('$1'), line('$1')).
expr  -> string_lit              : rufus_form:make_literal(string, list_to_binary(text('$1')), line('$1')).
expr  -> identifier              : rufus_form:make_identifier(list_to_atom(text('$1')), line('$1')).
expr  -> binary_op               : '$1'.
expr  -> cons                    : '$1'.
expr  -> match                   : '$1'.
expr  -> call                    : '$1'.
expr  -> list '[' type ']' '{' args '}' :
                                   rufus_form:make_literal(list, '$3', '$6', line('$1')).

binary_op -> expr '+' expr       : rufus_form:make_binary_op('+', '$1', '$3', line('$2')).
binary_op -> expr '-' expr       : rufus_form:make_binary_op('-', '$1', '$3', line('$2')).
binary_op -> expr '*' expr       : rufus_form:make_binary_op('*', '$1', '$3', line('$2')).
binary_op -> expr '/' expr       : rufus_form:make_binary_op('/', '$1', '$3', line('$2')).
binary_op -> expr '%' expr       : rufus_form:make_binary_op('%', '$1', '$3', line('$2')).
binary_op -> expr 'and' expr     : rufus_form:make_binary_op('and', '$1', '$3', line('$2')).
binary_op -> expr 'or' expr      : rufus_form:make_binary_op('or', '$1', '$3', line('$2')).

call -> identifier '(' args ')'  : rufus_form:make_call(list_to_atom(text('$1')), '$3', line('$1')).

cons -> list '[' type ']' '{' expr '|' expr '}' :
                                   rufus_form:make_cons('$3', '$6', '$8', line('$1')).
cons -> list '[' type ']' '{' expr '|' '{' args '}' '}' :
                                   rufus_form:make_cons('$3', '$6', rufus_form:make_literal(list, '$3', '$9', line('$1')), line('$1')).

match -> expr '=' expr           : rufus_form:make_match('$1', '$3', line('$2')).

Erlang code.

%% text returns the content from the token.
text({_TokenType, _Line, Text}) ->
    Text.

%% line returns the line number from the token.
line({_TokenType, Line}) ->
    Line;
line({_TokenType, Line, _Text}) ->
    Line.
