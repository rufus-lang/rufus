-module(rufus_apply_test).

-include_lib("eunit/include/eunit.hrl").

typecheck_and_annotate_with_function_calling_an_unknown_function_test() ->
    RufusText = "
    module example
    func Echo(text string) string { Ping() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_apply:typecheck_and_annotate(Forms),
    Data = #{args => [], spec => 'Ping'},
    ?assertEqual({error, unknown_func, Data}, Result).

typecheck_and_annotate_with_function_calling_a_function_with_a_missing_argument_test() ->
    RufusText = "
    module example
    func Echo(n string) string { \"Hello\" }
    func Broken() string { Echo() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_apply:typecheck_and_annotate(Forms),
    Data = #{arg_exprs => [],
             func_decls => [{func_decl, #{args => [{arg_decl, #{line => 3,
                                                                spec => n,
                                                                type => {type, #{line => 3,
                                                                                 source => rufus_text,
                                                                                 spec => string}}}}],
                                          exprs => [{string_lit, #{line => 3,
                                                                   spec => <<"Hello">>,
                                                                   type => {type, #{line => 3,
                                                                                    source => inferred,
                                                                                    spec => string}}}}],
                                          line => 3,
                                          return_type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => string}},
                                          spec => 'Echo'}}]},
    ?assertEqual({error, unknown_arity, Data}, Result).

typecheck_and_annotate_with_function_calling_a_function_with_a_mismatched_argument_type_test() ->
    RufusText = "
    module example
    func Echo(n string) string { \"Hello\" }
    func Broken() string { Echo(42) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_apply:typecheck_and_annotate(Forms),
    Data = #{arg_exprs => [{int_lit, #{line => 4,
                                       spec => 42,
                                       type => {type, #{line => 4,
                                                        source => inferred,
                                                        spec => int}}}}],
             func_decls => [{func_decl, #{args => [{arg_decl, #{line => 3,
                                                                spec => n,
                                                                type => {type, #{line => 3,
                                                                                 source => rufus_text,
                                                                                 spec => string}}}}],
                                          exprs => [{string_lit, #{line => 3,
                                                                   spec => <<"Hello">>,
                                                                   type => {type, #{line => 3,
                                                                                    source => inferred,
                                                                                    spec => string}}}}],
                                          line => 3,
                                          return_type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => string}},
                                          spec => 'Echo'}}]},
    ?assertEqual({error, unmatched_args, Data}, Result).

typecheck_and_annotate_with_function_calling_a_function_with_one_argument_test() ->
    RufusText = "
    module math
    func Echo(text string) string { text }
    func Greeting() string { Echo(\"hello\") }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms1} = rufus_scope:annotate_locals(Forms),
    {ok, AnnotatedForms2} = rufus_apply:typecheck_and_annotate(AnnotatedForms1),
    Expected = [{module,#{line => 2,
                          spec => math}},
                {func_decl, #{args => [{arg_decl, #{line => 3,
                                                    spec => text,
                                                    type => {type, #{line => 3,
                                                                     source => rufus_text,
                                                                     spec => string}}}}],
                              exprs => [{identifier, #{line => 3,
                                                       locals => #{text => {type, #{line => 3,
                                                                                    source => rufus_text,
                                                                                    spec => string}}},
                                                       spec => text}}],
                              line => 3,
                              return_type => {type, #{line => 3,
                                                      source => rufus_text,
                                                      spec => string}},
                              spec => 'Echo'}},
                {func_decl, #{args => [],
                              exprs => [{apply, #{args => [{string_lit, #{line => 4,
                                                                          spec => <<"hello">>,
                                                                          type => {type, #{line => 4,
                                                                                           source => inferred,
                                                                                           spec => string}}}}],
                                                  line => 4,
                                                  locals => #{text => {type, #{line => 3,
                                                                               source => rufus_text,
                                                                               spec => string}}},
                                                  spec => 'Echo',
                                                  type => {type, #{line => 3,
                                                                   source => rufus_text,
                                                                   spec => string}}}}],
                              line => 4,
                              return_type => {type, #{line => 4,
                                                      source => rufus_text,
                                                      spec => string}},
                              spec => 'Greeting'}}],
    ?assertEqual(Expected, AnnotatedForms2).
