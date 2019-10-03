-module(rufus_typecheck_apply_test).

-include_lib("eunit/include/eunit.hrl").

globals_without_func_forms_test() ->
    RufusText = "module empty",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, #{}}, rufus_typecheck_apply:globals(Forms)).

globals_test() ->
    RufusText = "
    module example
    func Echo(n string) string { n }
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Echo1 = {func_decl, #{args => [{arg_decl, #{line => 3,
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
    Number0 = {func_decl, #{args => [],
                            exprs => [{int_lit, #{line => 4,
                                                  spec => 42,
                                                  type => {type, #{line => 4,
                                                                   source => inferred,
                                                                   spec => int}}}}],
                            line => 4,
                            return_type => {type, #{line => 4,
                                                    source => rufus_text,
                                                    spec => int}},
                            spec => 'Number'}},
    ?assertEqual({ok, #{'Echo' => [Echo1], 'Number' => [Number0]}}, rufus_typecheck_apply:globals(Forms)).

globals_with_multiple_function_heads_test() ->
    RufusText = "
    module example
    func Echo(n string) string { n }
    func Echo(n int) int { n }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    EchoString = {func_decl, #{args => [{arg_decl, #{line => 3,
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
    EchoInt = {func_decl, #{args => [{arg_decl, #{line => 4,
                                                  spec => n,
                                                  type => {type, #{line => 4,
                                                                   source => rufus_text,
                                                                   spec => int}}}}],
                            exprs => [{identifier, #{line => 4,
                                                     spec => n}}],
                            line => 4,
                            return_type => {type, #{line => 4,
                                                    source => rufus_text,
                                                    spec => int}},
                            spec => 'Echo'}},
    ?assertEqual({ok, #{'Echo' => [EchoString, EchoInt]}}, rufus_typecheck_apply:globals(Forms)).
