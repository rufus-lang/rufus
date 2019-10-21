-module(rufus_typecheck_apply_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions calling an arity-0 function

forms_with_function_calling_a_function_without_arguments_test() ->
    RufusText = "
    module math
    func Four() int { 100 % 13 % 5 }
    func Random() int { Four() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms1} = rufus_annotate_locals:forms(Forms),
    {ok, AnnotatedForms2} = rufus_typecheck_binary_op:forms(AnnotatedForms1),
    {ok, AnnotatedForms3} = rufus_typecheck_apply:forms(AnnotatedForms2),
    Expected = [{module, #{line => 2,
                           spec => math}},
                {func_decl, #{args => [],
                              exprs => [{binary_op, #{left => {binary_op, #{left => {int_lit, #{line => 3,
                                                                                                spec => 100,
                                                                                                type => {type, #{line => 3,
                                                                                                                 source => inferred,
                                                                                                                 spec => int}}}},
                                                                            line => 3,
                                                                            op => '%',
                                                                            right => {int_lit, #{line => 3,
                                                                                                 spec => 13,
                                                                                                 type => {type, #{line => 3,
                                                                                                                  source => inferred,
                                                                                                                  spec => int}}}},
                                                                            type => {type, #{line => 3,
                                                                                             source => inferred,
                                                                                             spec => int}}}},
                                                      line => 3,
                                                      locals => #{},
                                                      op => '%',
                                                      right => {int_lit, #{line => 3,
                                                                           spec => 5,
                                                                           type => {type, #{line => 3,
                                                                                            source => inferred,
                                                                                            spec => int}}}},
                                                      type => {type, #{line => 3,
                                                                       source => inferred,
                                                                       spec => int}}}}],
                              line => 3,
                              return_type => {type, #{line => 3,
                                                      source => rufus_text,
                                                      spec => int}},
                              spec => 'Four'}},
                {func_decl, #{args => [],
                              exprs => [{apply, #{args => [],
                                                  line => 4,
                                                  locals => #{},
                                                  spec => 'Four',
                                                  type => {type, #{line => 3,
                                                                   source => rufus_text,
                                                                   spec => int}}}}],
                              line => 4,
                              return_type => {type, #{line => 4,
                                                      source => rufus_text,
                                                      spec => int}},
                              spec => 'Random'}}],
    ?assertEqual(Expected, AnnotatedForms3).

forms_with_function_calling_a_function_with_a_missing_argument_test() ->
    RufusText = "
    module example
    func Echo(n string) string { \"Hello\" }
    func Broken() string { Echo() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_typecheck_apply:forms(Forms),
    Data = #{arg_exprs => [],
             form_decls => [{func_decl, #{args => [{arg_decl, #{line => 3,
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

%% forms_with_function_calling_a_function_with_a_mismatched_argument_type_test() ->
%%     RufusText = "
%%     module example
%%     func Echo(n string) string { \"Hello\" }
%%     func Broken() string { Echo(42) }
%%     ",
%%     {ok, Tokens} = rufus_tokenize:string(RufusText),
%%     {ok, Forms} = rufus_parse:parse(Tokens),
%%     Result = rufus_typecheck_apply:forms(Forms),
%%     ?assertEqual({error, invalid_arg_type, #{}}, Result).
