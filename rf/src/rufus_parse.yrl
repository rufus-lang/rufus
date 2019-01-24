Nonterminals
  root decl expr function type arg args
  .

Terminals '(' ')' '{' '}' ',' func identifier import module bool bool_lit float float_lit int int_lit string string_lit.

Rootsymbol root.

root -> decl :
    ['$1'].
root -> decl root :
    ['$1'] ++ '$2'.

decl -> import string_lit :
    {import, #{line => token_line('$2'),
               spec => token_chars('$2')}
    }.
decl -> module identifier :
    {module, #{line => token_line('$2'),
               spec => list_to_atom(token_chars('$2'))}
    }.
decl -> function :
    '$1'.

function -> func identifier '(' args ')' type '{' expr '}' :
    {func, #{line => token_line('$1'),
             spec => list_to_atom(token_chars('$2')),
             args => '$4',
             return_type => '$6',
             exprs => '$8'}
    }.

type -> bool :
    {type, #{line => token_line('$1'),
             spec => bool}}.
type -> float :
    {type, #{line => token_line('$1'),
             spec => float}
    }.
type -> int :
    {type, #{line => token_line('$1'),
             spec => int}
    }.
type -> string :
    {type, #{line => token_line('$1'),
             spec => string}
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

expr -> bool_lit :
    [{bool_lit, #{line => token_line('$1'),
                  spec => token_chars('$1'),
                  type => {type, #{line => token_line('$1'), spec => bool}}}
    }].
expr -> float_lit :
    [{float_lit, #{line => token_line('$1'),
                   spec => token_chars('$1'),
                   type => {type, #{line => token_line('$1'), spec => float}}}
    }].
expr -> int_lit :
    [{int_lit, #{line => token_line('$1'),
                 spec => token_chars('$1'),
                 type => {type, #{line => token_line('$1'), spec => int}}}
    }].
expr -> string_lit :
    [{string_lit, #{line => token_line('$1'),
                    spec => list_to_binary(token_chars('$1')),
                    type => {type, #{line => token_line('$1'), spec => string}}}
    }].
expr -> identifier :
    [{identifier, #{line => token_line('$1'),
                    spec => list_to_atom(token_chars('$1'))}
    }].

Erlang code.

token_chars({_TokenType, _Line, Chars}) ->
    Chars.

token_line({_TokenType, Line}) ->
    Line;
token_line({_TokenType, Line, _Chars}) ->
    Line.
