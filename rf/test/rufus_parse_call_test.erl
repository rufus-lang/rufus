-module(rufus_parse_call_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions calling an arity-0 function

parse_function_calling_a_function_without_arguments_test() ->
    RufusText = "
    module math
    func Four() int { 100 % 13 % 5 }
    func Random() int { Four() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func, #{params => [],
               exprs => [{binary_op, #{left => {binary_op, #{op => '%',
                                                             left => {int_lit, #{line => 3,
                                                                                 spec => 100,
                                                                                 type => {type, #{line => 3,
                                                                                                  spec => int,
                                                                                                  source => inferred}}}},
                                                             right => {int_lit, #{line => 3,
                                                                                  spec => 13,
                                                                                  type => {type, #{line => 3,
                                                                                                   spec => int,
                                                                                                   source => inferred}}}},
                                                             line => 3}},
                                       line => 3,
                                       op => '%',
                                       right => {int_lit, #{line => 3,
                                                            spec => 5,
                                                            type => {type, #{line => 3,
                                                                             spec => int,
                                                                             source => inferred}}}}}}],
               line => 3,
               return_type => {type, #{line => 3,
                                       spec => int,
                                       source => rufus_text}},
               spec => 'Four'}},
      {func, #{params => [],
               exprs => [{call, #{args => [], line => 4, spec => 'Four'}}],
               line => 4,
               return_type => {type, #{line => 4,
                                       source => rufus_text,
                                       spec => int}},
               spec => 'Random'}}
    ], Forms).

%% Arity-0 functions calling an arity-1 function

parse_function_calling_a_function_with_an_argument_test() ->
    RufusText = "
    module example
    func Echo(n string) string { n }
    func Echo() string { Echo(\"Hello\") }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => example}},
      {func, #{params => [{param, #{line => 3,
                                    spec => n,
                                    type => {type, #{line => 3,
                                                     source => rufus_text,
                                                     spec => string}}}}],
               exprs => [{identifier, #{line => 3,
                                        spec => n}}],
               line => 3,
               return_type => {type, #{line => 3,
                                       source => rufus_text,
                                       spec => string}},
               spec => 'Echo'}},
      {func, #{params => [],
               exprs => [{call, #{args => [{string_lit, #{line => 4,
                                                          spec => <<"Hello">>,
                                                          type => {type, #{line => 4,
                                                                           source => inferred,
                                                                           spec => string}}}}],
                                  line => 4,
                                  spec => 'Echo'}}],
               line => 4,
               return_type => {type, #{line => 4,
                                       source => rufus_text,
                                       spec => string}},
               spec => 'Echo'}}
    ], Forms).
