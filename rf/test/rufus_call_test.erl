-module(rufus_call_test).

-include_lib("eunit/include/eunit.hrl").

typecheck_and_annotate_with_function_calling_an_unknown_function_test() ->
    RufusText = "
    module example
    func Echo(text string) string { Ping() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_call:typecheck_and_annotate(Forms),
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
    Result = rufus_call:typecheck_and_annotate(Forms),
    Data = #{arg_exprs => [],
             func_decls => [{func_decl, #{params => [{param, #{line => 3,
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
    Result = rufus_call:typecheck_and_annotate(Forms),
    Data = #{arg_exprs => [{int_lit, #{line => 4,
                                       spec => 42,
                                       type => {type, #{line => 4,
                                                        source => inferred,
                                                        spec => int}}}}],
             func_decls => [{func_decl, #{params => [{param, #{line => 3,
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
    {ok, AnnotatedForms2} = rufus_call:typecheck_and_annotate(AnnotatedForms1),
    Expected = [{module, #{line => 2,
                           spec => math}},
                {func_decl, #{exprs => [{identifier, #{line => 3,
                                                       locals => #{text => {type, #{line => 3,
                                                                                    source => rufus_text,
                                                                                    spec => string}}},
                                                       spec => text}}],
                              line => 3,
                              params => [{param, #{line => 3,
                                                   spec => text,
                                                   type => {type, #{line => 3,
                                                                    source => rufus_text,
                                                                    spec => string}}}}],
                              return_type => {type, #{line => 3,
                                                      source => rufus_text,
                                                      spec => string}},
                              spec => 'Echo'}},
                {func_decl, #{exprs => [{call, #{args => [{string_lit, #{line => 4,
                                                                         spec => <<"hello">>,
                                                                         type => {type, #{line => 4,
                                                                                          source => inferred,
                                                                                          spec => string}}}}],
                                                 line => 4,
                                                 spec => 'Echo',
                                                 type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => string}}}}],
                              line => 4,
                              params => [],
                              return_type => {type, #{line => 4,
                                                      source => rufus_text,
                                                      spec => string}},
                              spec => 'Greeting'}}],
    ?assertEqual(Expected, AnnotatedForms2).

typecheck_and_annotate_with_function_calling_a_function_with_two_arguments_test() ->
    RufusText = "
    module math
    func Sum(m int, n int) int { m + n }
    func Random() int { Sum(1, 2) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms1} = rufus_scope:annotate_locals(Forms),
    {ok, AnnotatedForms2} = rufus_call:typecheck_and_annotate(AnnotatedForms1),
    Expected = [{module, #{line => 2,
                           spec => math}},
                {func_decl, #{exprs => [{binary_op, #{left => {identifier, #{line => 3,
                                                                             locals => #{m => {type, #{line => 3,
                                                                                                       source => rufus_text,
                                                                                                       spec => int}},
                                                                                         n => {type, #{line => 3,
                                                                                                       source => rufus_text,
                                                                                                       spec => int}}},
                                                                             spec => m}},
                                                      line => 3,
                                                      op => '+',
                                                      right => {identifier, #{line => 3,
                                                                              locals => #{m => {type, #{line => 3,
                                                                                                        source => rufus_text,
                                                                                                        spec => int}},
                                                                                          n => {type, #{line => 3,
                                                                                                        source => rufus_text,
                                                                                                        spec => int}}},
                                                                              spec => n}}}}],
                              line => 3,
                              params => [{param, #{line => 3,
                                                   spec => m,
                                                   type => {type, #{line => 3,
                                                                    source => rufus_text,
                                                                    spec => int}}}},
                                         {param, #{line => 3,
                                                   spec => n,
                                                   type => {type, #{line => 3,
                                                                    source => rufus_text,
                                                                    spec => int}}}}],
                              return_type => {type, #{line => 3,
                                                      source => rufus_text,
                                                      spec => int}},
                              spec => 'Sum'}},
                {func_decl, #{exprs => [{call, #{args => [{int_lit, #{line => 4,
                                                                      spec => 1,
                                                                      type => {type, #{line => 4,
                                                                                       source => inferred,
                                                                                       spec => int}}}},
                                                          {int_lit, #{line => 4,
                                                                      spec => 2,
                                                                      type => {type, #{line => 4,
                                                                                       source => inferred,
                                                                                       spec => int}}}}],
                                                 line => 4,
                                                 spec => 'Sum',
                                                 type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => int}}}}],
                              line => 4,
                              params => [],
                              return_type => {type, #{line => 4,
                                                      source => rufus_text,
                                                      spec => int}},
                              spec => 'Random'}}],
    ?assertEqual(Expected,  AnnotatedForms2).
