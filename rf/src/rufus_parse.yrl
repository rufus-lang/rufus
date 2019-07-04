Nonterminals root decl expr function type arg args binary_op.

Terminals '(' ')' '{' '}' ',' '+' func identifier import module bool bool_lit float float_lit int int_lit string string_lit.

Rootsymbol root.

Left 100 '+'.

root -> decl :
    ['$1'].
root -> decl root :
    ['$1'] ++ '$2'.

%% Module-level declarations

decl -> module identifier :
    {module, #{line => token_line('$2'),
               spec => list_to_atom(token_chars('$2'))}
    }.
decl -> import string_lit :
    {import, #{line => token_line('$2'),
               spec => token_chars('$2')}
    }.
decl -> function :
    '$1'.

%% Scalar types

type -> bool : rufus_form:make_user_specified_type(bool, token_line('$1')).
type -> float : rufus_form:make_user_specified_type(float, token_line('$1')).
type -> int : rufus_form:make_user_specified_type(int, token_line('$1')).
type -> string : rufus_form:make_user_specified_type(string, token_line('$1')).

%% Type literals

expr -> bool_lit : [rufus_form:make_literal(bool, token_chars('$1'), token_line('$1'))].
expr -> float_lit : [rufus_form:make_literal(float, token_chars('$1'), token_line('$1'))].
expr -> int_lit : [rufus_form:make_literal(int, token_chars('$1'), token_line('$1'))].
expr -> string_lit : [rufus_form:make_literal(string, list_to_binary(token_chars('$1')), token_line('$1'))].

expr -> identifier :
    [{identifier, #{line => token_line('$1'),
                    spec => list_to_atom(token_chars('$1'))}
    }].

expr -> binary_op :
    ['$1'].

%% Binary operations

binary_op -> expr '+' expr :
    {binary_op, #{line => token_line('$2'),
                  op => '+',
                  left => '$1',
                  right => '$3'}}.

%% Function declarations

function -> func identifier '(' args ')' type '{' expr '}' :
    {func, #{line => token_line('$1'),
             spec => list_to_atom(token_chars('$2')),
             args => '$4',
             return_type => '$6',
             exprs => '$8'}
    }.
args -> arg args :
    ['$1'|'$2'].
args -> '$empty' :
    [].
arg -> identifier type ',' :
    {arg, #{line => token_line('$1'),
            spec => list_to_atom(token_chars('$1')),
            type => '$2'}
    }.
arg -> identifier type :
    {arg, #{line => token_line('$1'),
            spec => list_to_atom(token_chars('$1')),
            type => '$2'}
    }.

Erlang code.

token_chars({_TokenType, _Line, Chars}) ->
    Chars.

token_line({_TokenType, Line}) ->
    Line;
token_line({_TokenType, Line, _Chars}) ->
    Line.
