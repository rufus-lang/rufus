%%
%% Token categories
%%

Nonterminals
    root decl
    type types block param params args
    expr exprs literal_expr throw_expr
    case_match_clause case_match_clauses
    catch_expr catch_match_clause catch_match_clauses
    binary_op call cons match_op match_op_param
    list_lit list_type.

Terminals
    '[' ']' '{' '}' '(' ')' '|'
    '+' '-' '*' '/' '%'
    ',' ';' '='
    'and' 'or'
    '==' '!='
    '<' '<=' '>' '>='
    '->'
    module import
    func identifier
    try catch after throw
    case match
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

Left     70 '*'.
Left     70 '/'.
Left     70 '%'.
Left     60 '+'.
Left     60 '-'.
Nonassoc 50 '=='.
Nonassoc 50 '!='.
Nonassoc 50 '<'.
Nonassoc 50 '<='.
Nonassoc 50 '>'.
Nonassoc 50 '>='.
Left     40 'and'.
Left     30 'or'.
Left     20 '='.

%% Give exprs higher precedence than catch_match_clause to prevent collisions
%% between a bare catch and a catch with one or more matchers.
Left     10 exprs.

%%
%% Grammar rules
%%

root -> decl                     : ['$1'].
root -> decl root                : ['$1'] ++ '$2'.

decl -> module identifier ';'    : rufus_form:make_module(list_to_atom(text('$2')), line('$2')).
decl -> import string_lit ';'    : rufus_form:make_import(text('$2'), line('$2')).
decl -> func identifier '(' params ')' type block :
                                   rufus_form:make_func(list_to_atom(text('$2')), '$4', '$6', '$7', line('$1')).

args  -> expr ',' args           : ['$1'|'$3'].
args  -> expr                    : ['$1'].
args  -> '$empty'                : [].

binary_op -> expr '+' expr       : rufus_form:make_binary_op('+', '$1', '$3', line('$2')).
binary_op -> expr '-' expr       : rufus_form:make_binary_op('-', '$1', '$3', line('$2')).
binary_op -> expr '*' expr       : rufus_form:make_binary_op('*', '$1', '$3', line('$2')).
binary_op -> expr '/' expr       : rufus_form:make_binary_op('/', '$1', '$3', line('$2')).
binary_op -> expr '%' expr       : rufus_form:make_binary_op('%', '$1', '$3', line('$2')).
binary_op -> expr 'and' expr     : rufus_form:make_binary_op('and', '$1', '$3', line('$2')).
binary_op -> expr 'or' expr      : rufus_form:make_binary_op('or', '$1', '$3', line('$2')).
binary_op -> expr '==' expr      : rufus_form:make_binary_op('==', '$1', '$3', line('$2')).
binary_op -> expr '!=' expr      : rufus_form:make_binary_op('!=', '$1', '$3', line('$2')).
binary_op -> expr '<' expr       : rufus_form:make_binary_op('<', '$1', '$3', line('$2')).
binary_op -> expr '<=' expr      : rufus_form:make_binary_op('<=', '$1', '$3', line('$2')).
binary_op -> expr '>' expr       : rufus_form:make_binary_op('>', '$1', '$3', line('$2')).
binary_op -> expr '>=' expr      : rufus_form:make_binary_op('>=', '$1', '$3', line('$2')).

block -> '{' exprs '}' ';'       : '$2'.

call -> identifier '(' args ')'  : rufus_form:make_call(list_to_atom(text('$1')), '$3', line('$1')).
cons -> list_type '{' expr '|' expr '}' :
                                   rufus_form:make_cons('$1', '$3', '$5', line('$1')).
cons -> list_type '{' expr '|' '{' args '}' '}' :
                                   rufus_form:make_cons('$1', '$3', rufus_form:make_literal(list, '$1', '$6', line('$6')), line('$1')).

literal_expr -> atom_lit         : rufus_form:make_literal(atom, text('$1'), line('$1')).
literal_expr -> bool_lit         : rufus_form:make_literal(bool, text('$1'), line('$1')).
literal_expr -> float_lit        : rufus_form:make_literal(float, text('$1'), line('$1')).
literal_expr -> int_lit          : rufus_form:make_literal(int, text('$1'), line('$1')).
literal_expr -> string_lit       : rufus_form:make_literal(string, list_to_binary(text('$1')), line('$1')).

catch_expr -> param              : '$1'.
catch_expr -> identifier         : rufus_form:make_identifier(list_to_atom(text('$1')), line('$1')).

expr -> literal_expr             : '$1'.
expr -> identifier               : rufus_form:make_identifier(list_to_atom(text('$1')), line('$1')).
expr -> binary_op                : '$1'.
expr -> cons                     : '$1'.
expr -> match_op                 : '$1'.
expr -> call                     : '$1'.
expr -> list_lit                 : '$1'.
expr -> func '(' params ')' type '{' exprs '}' :
                                   rufus_form:make_func('$3', '$5', '$7', line('$1')).
expr -> try '{' exprs '}' after '{' exprs '}' :
                                   rufus_form:make_try_catch_after('$3', [], '$7', line('$1')).
expr -> try '{' exprs '}' catch catch_expr '{' exprs '}' after '{' exprs '}' :
                                   rufus_form:make_try_catch_after('$3', [rufus_form:make_catch_clause('$6', '$8', line('$5'))], '$12', line('$1')).
expr -> try '{' exprs '}' catch catch_expr '{' exprs '}' :
                                   rufus_form:make_try_catch_after('$3', [rufus_form:make_catch_clause('$6', '$8', line('$5'))], [], line('$1')).
expr -> try '{' exprs '}' catch '{' exprs '}' after '{' exprs '}':
                                   rufus_form:make_try_catch_after('$3', [rufus_form:make_catch_clause('$7', line('$5'))], '$11', line('$1')).
expr -> try '{' exprs '}' catch '{' exprs '}' :
                                   rufus_form:make_try_catch_after('$3', [rufus_form:make_catch_clause('$7', line('$5'))], [], line('$1')).
expr -> try '{' exprs '}' catch '{' catch_match_clauses '}' after '{' exprs '}' :
                                   rufus_form:make_try_catch_after('$3', '$7', '$11', line('$1')).
expr -> try '{' exprs '}' catch '{' catch_match_clauses '}' :
                                   rufus_form:make_try_catch_after('$3', '$7', [], line('$1')).
expr -> case expr '{' case_match_clauses '}' :
                                   rufus_form:make_case('$2', '$4', line('$1')).

throw_expr -> throw expr         : rufus_form:make_throw('$2', line('$1')).

catch_match_clause -> match param '->' exprs :
                                   rufus_form:make_catch_clause('$2', '$4', line('$1')).
catch_match_clause -> match identifier '->' exprs :
                                   rufus_form:make_catch_clause(rufus_form:make_identifier(list_to_atom(text('$2')), line('$2')), '$4', line('$1')).

catch_match_clauses -> catch_match_clause catch_match_clauses : ['$1'|'$2'].
catch_match_clauses -> '$empty'  : [].

case_match_clause -> match param '->' exprs :
                                   rufus_form:make_case_clause('$2', '$4', line('$1')).
case_match_clause -> match identifier '->' exprs :
                                   rufus_form:make_case_clause(rufus_form:make_identifier(list_to_atom(text('$2')), line('$2')), '$4', line('$1')).

case_match_clauses -> case_match_clause case_match_clauses : ['$1'|'$2'].
case_match_clauses -> '$empty'  : [].

exprs -> expr ';' exprs          : ['$1'|'$3'].
exprs -> expr                    : ['$1'].
exprs -> throw_expr ';' exprs    : ['$1'|'$3'].
exprs -> throw_expr              : ['$1'].
exprs -> '$empty'                : [].

list_lit -> list_type '{' args '}' :
                                   rufus_form:make_literal(list, '$1', '$3', line('$1')).

list_type -> list '[' type ']'   : rufus_form:make_type(list, '$3', line('$1')).

match_op -> expr '=' expr        : rufus_form:make_match_op('$1', '$3', line('$2')).
match_op_param -> expr '=' param : rufus_form:make_match_op('$1', '$3', line('$2')).

param -> identifier type         : rufus_form:make_param(list_to_atom(text('$1')), '$2', line('$1')).
param -> atom_lit                : rufus_form:make_literal(atom, text('$1'), line('$1')).
param -> bool_lit                : rufus_form:make_literal(bool, text('$1'), line('$1')).
param -> cons                    : '$1'.
param -> float_lit               : rufus_form:make_literal(float, text('$1'), line('$1')).
param -> int_lit                 : rufus_form:make_literal(int, text('$1'), line('$1')).
param -> list_lit                : '$1'.
param -> string_lit              : rufus_form:make_literal(string, list_to_binary(text('$1')), line('$1')).
param -> match_op_param          : '$1'.

params -> param ',' params       : ['$1'|'$3'].
params -> param                  : ['$1'].
params -> '$empty'               : [].

type -> atom                     : rufus_form:make_type(atom, line('$1')).
type -> bool                     : rufus_form:make_type(bool, line('$1')).
type -> float                    : rufus_form:make_type(float, line('$1')).
type -> int                      : rufus_form:make_type(int, line('$1')).
type -> string                   : rufus_form:make_type(string, line('$1')).
type -> list_type                : '$1'.
type -> func '(' types ')' type  : rufus_form:make_type(func, '$3', '$5', line('$1')).

types -> type types              : ['$1'|'$2'].
types -> '$empty'                : [].

Erlang code.

%% text returns the content from the token.
text({_TokenType, _Line, Text}) ->
    Text.

%% line returns the line number from the token.
line([{_TokenType, #{line := Line}} | _]) ->
    Line;
line({_TokenType, #{line := Line}}) ->
    Line;
line({_TokenType, Line}) ->
    Line;
line({_TokenType, Line, _Text}) ->
    Line.
