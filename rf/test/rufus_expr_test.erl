-module(rufus_expr_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_with_function_calling_an_unknown_function_test() ->
    RufusText = "
    module example
    func Echo(text string) string { Ping() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_expr:typecheck_and_annotate(Forms),
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
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{args => [],
             funcs => [{func, #{params => [{param, #{line => 3,
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
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{args => [{int_lit, #{line => 4,
                                       spec => 42,
                                       type => {type, #{line => 4,
                                                        source => inferred,
                                                        spec => int}}}}],
             funcs => [{func, #{params => [{param, #{line => 3,
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

typecheck_and_annotate_with_empty_module_test() ->
    RufusText = "module empty",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{params => [],
                 exprs => [{int_lit, #{line => 3, spec => 42,
                                       type => {type, #{line => 3, spec => int, source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3, spec => int, source => rufus_text}},
                 spec => 'Number'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_function_calling_a_function_with_one_argument_test() ->
    RufusText = "
    module math
    func Echo(text string) string { text }
    func Greeting() string { Echo(\"hello\") }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => math}},
                {func, #{exprs => [{identifier, #{line => 3,
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
                {func, #{exprs => [{call, #{args => [{string_lit, #{line => 4,
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
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_with_function_calling_a_function_with_two_arguments_test() ->
    RufusText = "
    module math
    func Sum(m int, n int) int { m + n }
    func Random() int { Sum(1, 2) }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => math}},
                {func, #{exprs => [{binary_op, #{left => {identifier, #{line => 3,
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
                                                                         spec => n}},
                                                 type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => int}}}}],
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
                {func, #{exprs => [{call, #{args => [{int_lit, #{line => 4,
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
    ?assertEqual(Expected,  AnnotatedForms).

%% Arity-1 functions taking an argument and returning a literal

typecheck_and_annotate_for_function_taking_an_atom_and_returning_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping(m atom) atom { :pong }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => m,
                                      type => {type, #{line => 3,
                                                       spec => atom,
                                                       source => rufus_text}}}}],
                 exprs => [{atom_lit, #{line => 3,
                                        spec => pong,
                                        type => {type, #{line => 3,
                                                         spec => atom,
                                                         source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => atom,
                                         source => rufus_text}},
                 spec => 'Ping'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(b bool) bool { true }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => b,
                                      type => {type, #{line => 3,
                                                       spec => bool,
                                                       source => rufus_text}}}}],
                 exprs => [{bool_lit, #{line => 3,
                                        spec => true,
                                        type => {type, #{line => 3,
                                                         spec => bool,
                                                         source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => bool,
                                         source => rufus_text}},
                 spec => 'MaybeEcho'}
        }
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => n,
                                      type => {type, #{line => 3,
                                                       spec => float,
                                                       source => rufus_text}}}}],
                 exprs => [{float_lit, #{line => 3,
                                         spec => 3.14159265359,
                                         type => {type, #{line => 3,
                                                          spec => float,
                                                          source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => float,
                                         source => rufus_text}},
                 spec => 'MaybeEcho'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n int) int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => n,
                                      type => {type, #{line => 3,
                                                       spec => int,
                                                       source => rufus_text}}}}],
                 exprs => [{int_lit, #{line => 3,
                                       spec => 42,
                                       type => {type, #{line => 3,
                                                        spec => int,
                                                        source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => int,
                                         source => rufus_text}},
                 spec => 'MaybeEcho'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(s string) string { \"Hello\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => s,
                                      type => {type, #{line => 3,
                                                       spec => string,
                                                       source => rufus_text}}}}],
                 exprs => [{string_lit, #{line => 3,
                                          spec => <<"Hello">>,
                                          type => {type, #{line => 3,
                                                           spec => string,
                                                           source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => string,
                                         source => rufus_text}},
                 spec => 'MaybeEcho'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Arity-1 functions taking and returning an argument

typecheck_and_annotate_for_function_taking_an_atom_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(b atom) atom { b }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => b,
                                      type => {type, #{line => 3,
                                                       spec => atom,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          locals => #{b => {type, #{line => 3,
                                                                    spec => atom,
                                                                    source => rufus_text}}},
                                          spec => b}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => atom,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_bool_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(b bool) bool { b }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => b,
                                      type => {type, #{line => 3,
                                                       spec => bool,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          locals => #{b => {type, #{line => 3,
                                                                    spec => bool,
                                                                    source => rufus_text}}},
                                          spec => b}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => bool,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_float_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(n float) float { n }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => n,
                                      type => {type, #{line => 3,
                                                       spec => float,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          locals => #{n => {type, #{line => 3,
                                                                    spec => float,
                                                                    source => rufus_text}}},
                                          spec => n}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => float,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_an_int_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => n,
                                      type => {type, #{line => 3,
                                                       spec => int,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          locals => #{n => {type, #{line => 3,
                                                                    spec => int,
                                                                    source => rufus_text}}},
                                          spec => n}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => int,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_string_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(s string) string { s }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => s,
                                      type => {type, #{line => 3,
                                                       spec => string,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          locals => #{s => {type, #{line => 3,
                                                                    spec => string,
                                                                    source => rufus_text}}},
                                          spec => s}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => string,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

%% binary_op tests

typecheck_and_annotate_binary_op_with_ints_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 19 + 23 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{params => [],
                         exprs => [{binary_op, #{left => {int_lit, #{line => 3,
                                                                     spec => 19,
                                                                     type => {type, #{line => 3,
                                                                                      source => inferred,
                                                                                      spec => int}}}},
                                                 line => 3,
                                                 op => '+',
                                                 right => {int_lit, #{line => 3,
                                                                      spec => 23,
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
                         spec => 'FortyTwo'}}],
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_binary_op_with_floats_test() ->
    RufusText = "
    module example
    func Pi() float { 1.0 + 2.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{params => [],
                         exprs => [{binary_op, #{left => {float_lit, #{line => 3,
                                                                       spec => 1.0,
                                                                       type => {type, #{line => 3,
                                                                                        source => inferred,
                                                                                        spec => float}}}},
                                                 line => 3,
                                                 op => '+',
                                                 right => {float_lit, #{line => 3,
                                                                        spec => 2.14159265359,
                                                                        type => {type, #{line => 3,
                                                                                         source => inferred,
                                                                                         spec => float}}}},
                                                 type => {type, #{line => 3,
                                                                  source => inferred,
                                                                  spec => float}}}}],
                         line => 3,
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => float}},
                         spec => 'Pi'}}],
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_binary_op_with_float_and_int_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 19.0 + 23 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = #{form => {binary_op, #{left => {float_lit, #{line => 3,
                                                             spec => 19.0,
                                                             type => {type, #{line => 3,
                                                                              source => inferred,
                                                                              spec => float}}}},
                                       line => 3,
                                       locals => #{},
                                       op => '+',
                                       right => {int_lit, #{line => 3,
                                                            spec => 23,
                                                            type => {type, #{line => 3,
                                                                             source => inferred,
                                                                             spec => int}}}}}}},
    {error, unmatched_operand_type, Data} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, Data).

typecheck_and_annotate_binary_op_with_float_and_float_and_int_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 13.0 + 6.0 + 23 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = #{form => {binary_op, #{left => {binary_op, #{left => {float_lit, #{line => 3,
                                                                                   spec => 13.0,
                                                                                   type => {type, #{line => 3,
                                                                                                    source => inferred,
                                                                                                    spec => float}}}},
                                                             line => 3,
                                                             op => '+',
                                                             right => {float_lit, #{line => 3,
                                                                                    spec => 6.0,
                                                                                    type => {type, #{line => 3,
                                                                                                     source => inferred,
                                                                                                     spec => float}}}},
                                                             type => {type, #{line => 3,
                                                                              source => inferred,
                                                                              spec => float}}}},
                                       line => 3,
                                       locals => #{},
                                       op => '+',
                                       right => {int_lit, #{line => 3,
                                                            spec => 23,
                                                            type => {type, #{line => 3,
                                                                             source => inferred,
                                                                             spec => int}}}}}}},
    {error, unmatched_operand_type, Data} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, Data).

typecheck_and_annotate_binary_op_with_bools_test() ->
    RufusText = "
    module example
    func Concat() bool { true + false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = #{form => {binary_op, #{left => {bool_lit, #{line => 3,
                                                            spec => true,
                                                            type => {type, #{line => 3,
                                                                             source => inferred,
                                                                             spec => bool}}}},
                                       line => 3,
                                       locals => #{},
                                       op => '+',
                                       right => {bool_lit, #{line => 3,
                                                             spec => false,
                                                             type => {type, #{line => 3,
                                                                              source => inferred,
                                                                              spec => bool}}}}}}},
    {error, unsupported_operand_type, Data} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, Data).

typecheck_and_annotate_binary_op_with_strings_test() ->
    RufusText = "
    module example
    func Concat() string { \"port\" + \"manteau\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = #{form => {binary_op, #{left => {string_lit, #{line => 3,
                                                              spec => <<"port">>,
                                                              type => {type, #{line => 3,
                                                                               source => inferred,
                                                                               spec => string}}}},
                                       line => 3,
                                       locals => #{},
                                       op => '+',
                                       right => {string_lit, #{line => 3,
                                                               spec => <<"manteau">>,
                                                               type => {type, #{line => 3,
                                                                                source => inferred,
                                                                                spec => string}}}}}}},
    {error, Reason, Data} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(unsupported_operand_type, Reason),
    ?assertEqual(Expected, Data).

typecheck_and_annotate_remainder_op_with_ints_test() ->
    RufusText = "
    module example
    func Six() int { 27 % 7 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{params => [],
                         exprs => [{binary_op, #{left => {int_lit, #{line => 3,
                                                                     spec => 27,
                                                                     type => {type, #{line => 3,
                                                                                      source => inferred,
                                                                                      spec => int}}}},
                                                 line => 3,
                                                 op => '%',
                                                 right => {int_lit, #{line => 3,
                                                                      spec => 7,
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
                         spec => 'Six'}}],
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_remainder_op_with_floats_test() ->
    RufusText = "
    module example
    func Six() int { 27.0 % 7.0 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = #{form => {binary_op, #{left => {float_lit, #{line => 3,
                                                             spec => 27.0,
                                                             type => {type, #{line => 3,
                                                                              source => inferred,
                                                                              spec => float}}}},
                                       line => 3,
                                       locals => #{},
                                       op => '%',
                                       right => {float_lit, #{line => 3,
                                                              spec => 7.0,
                                                              type => {type, #{line => 3,
                                                                               source => inferred,
                                                                               spec => float}}}}}}},
    {error, Reason, Data} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(unsupported_operand_type, Reason),
    ?assertEqual(Expected, Data).

%% Function return type checking tests

typecheck_and_annotate_function_with_literal_return_value_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_unmatched_return_types_test() ->
    RufusText = "
    module example
    func Number() int { 42.0 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = {error, unmatched_return_type, #{expr => {float_lit, #{line => 3,
                                                                      spec => 42.0,
                                                                      type => {type, #{line => 3,
                                                                                       source => inferred,
                                                                                       spec => float}}}},
                                                return_type => {type, #{line => 3,
                                                                        source => rufus_text,
                                                                        spec => int}}}},
    ?assertEqual(Expected, rufus_expr:typecheck_and_annotate(Forms)).

%% match tests

typecheck_and_annotate_function_with_a_match_that_binds_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping() atom {
        response = :pong
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response,
                                                                    type => {type, #{line => 4,
                                                                                     source => inferred,
                                                                                     spec => atom}}}},
                                             line => 4,
                                             right => {atom_lit, #{line => 4,
                                                                   spec => pong,
                                                                   type => {type, #{line => 4,
                                                                                    source => inferred,
                                                                                    spec => atom}}}}}},
                                   {identifier, #{line => 5,
                                                  locals => #{response => {type, #{line => 4,
                                                                                   source => inferred,
                                                                                   spec => atom}}},
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => atom}},
                         spec => 'Ping'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_bool_literal_test() ->
    RufusText = "
    module example
    func Truthy() bool {
        response = true
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response,
                                                                    type => {type, #{line => 4,
                                                                                     source => inferred,
                                                                                     spec => bool}}}},
                                             line => 4,
                                             right => {bool_lit, #{line => 4,
                                                                   spec => true,
                                                                   type => {type, #{line => 4,
                                                                                    source => inferred,
                                                                                    spec => bool}}}}}},
                                   {identifier, #{line => 5,
                                                  locals => #{response => {type, #{line => 4,
                                                                                   source => inferred,
                                                                                   spec => bool}}},
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => bool}},
                         spec => 'Truthy'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText = "
    module example
    func FortyTwo() float {
        response = 42.0
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response,
                                                                    type => {type, #{line => 4,
                                                                                     source => inferred,
                                                                                     spec => float}}}},
                                             line => 4,
                                             right => {float_lit, #{line => 4,
                                                                   spec => 42.0,
                                                                   type => {type, #{line => 4,
                                                                                    source => inferred,
                                                                                    spec => float}}}}}},
                                   {identifier, #{line => 5,
                                                  locals => #{response => {type, #{line => 4,
                                                                                   source => inferred,
                                                                                   spec => float}}},
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => float}},
                         spec => 'FortyTwo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_an_int_literal_test() ->
    RufusText = "
    module example
    func FortyTwo() int {
        response = 42
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response,
                                                                    type => {type, #{line => 4,
                                                                                     source => inferred,
                                                                                     spec => int}}}},
                                             line => 4,
                                             right => {int_lit, #{line => 4,
                                                                  spec => 42,
                                                                  type => {type, #{line => 4,
                                                                                   source => inferred,
                                                                                   spec => int}}}}}},
                                   {identifier, #{line => 5,
                                                  locals => #{response => {type, #{line => 4,
                                                                                   source => inferred,
                                                                                   spec => int}}},
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => int}},
                         spec => 'FortyTwo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText = "
    module example
    func Ping() string {
        response = \"pong\"
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response,
                                                                    type => {type, #{line => 4,
                                                                                     source => inferred,
                                                                                     spec => string}}}},
                                             line => 4,
                                             right => {string_lit, #{line => 4,
                                                                     spec => <<"pong">>,
                                                                     type => {type, #{line => 4,
                                                                                      source => inferred,
                                                                                      spec => string}}}}}},
                                   {identifier, #{line => 5,
                                                  locals => #{response => {type, #{line => 4,
                                                                                   source => inferred,
                                                                                   spec => string}}},
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => string}},
                         spec => 'Ping'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_function_with_a_match_that_has_an_unbound_variable_test() ->
    RufusText = "
    module example
    func Broken() int {
        value = 1
        value = unbound
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{form => {identifier, #{line => 5,
                                    locals => #{value => {type, #{line => 4,
                                                                  source => inferred,
                                                                  spec => int}}},
                                    spec => unbound}},
             globals => #{'Broken' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                                           spec => value}},
                                                                    line => 4,
                                                                    right => {int_lit, #{line => 4,
                                                                                         spec => 1,
                                                                                         type => {type, #{line => 4,
                                                                                                          source => inferred,
                                                                                                          spec => int}}}}}},
                                                          {match, #{left => {identifier, #{line => 5,
                                                                                           spec => value}},
                                                                    line => 5,
                                                                    right => {identifier, #{line => 5,
                                                                                            spec => unbound}}}}],
                                                line => 3,
                                                params => [],
                                                return_type => {type, #{line => 3,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Broken'}}]},
             locals => #{value => {type, #{line => 4,
                                           source => inferred,
                                           spec => int}}}},
    ?assertEqual({error, unbound_variable, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_unbound_variables_test() ->
    RufusText = "
    module example
    func Broken() int {
        unbound1 = unbound2
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{left => #{form => {identifier, #{line => 4,
                                              locals => #{},
                                              spec => unbound1}},
                       globals => #{'Broken' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                                                     spec => unbound1}},
                                                                              line => 4,
                                                                              right => {identifier, #{line => 4,
                                                                                                      spec => unbound2}}}}],
                                                          line => 3,
                                                          params => [],
                                                          return_type => {type, #{line => 3,
                                                                                  source => rufus_text,
                                                                                  spec => int}},
                                                          spec => 'Broken'}}]},
                       locals => #{}},
             right => #{form => {identifier, #{line => 4,
                                               locals => #{},
                                               spec => unbound2}},
                        globals => #{'Broken' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                                                      spec => unbound1}},
                                                                               line => 4,
                                                                               right => {identifier, #{line => 4,
                                                                                                       spec => unbound2}}}}],
                                                           line => 3,
                                                           params => [],
                                                           return_type => {type, #{line => 3,
                                                                                   source => rufus_text,
                                                                                   spec => int}},
                                                           spec => 'Broken'}}]},
                        locals => #{}}},
    ?assertEqual({error, unbound_variables, Data}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_a_match_that_has_unmatched_types_test() ->
    RufusText = "
    module example
    func Broken() int {
        a = :hello
        i = 42
        a = i
        i
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Data = #{globals => #{'Broken' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                                           spec => a}},
                                                                    line => 4,
                                                                    right => {atom_lit, #{line => 4,
                                                                                          spec => hello,
                                                                                          type => {type, #{line => 4,
                                                                                                           source => inferred,
                                                                                                           spec => atom}}}}}},
                                                          {match, #{left => {identifier, #{line => 5,
                                                                                           spec => i}},
                                                                    line => 5,
                                                                    right => {int_lit, #{line => 5,
                                                                                         spec => 42,
                                                                                         type => {type, #{line => 5,
                                                                                                          source => inferred,
                                                                                                          spec => int}}}}}},
                                                          {match, #{left => {identifier, #{line => 6,
                                                                                           spec => a}},
                                                                    line => 6,
                                                                    right => {identifier, #{line => 6,
                                                                                            spec => i}}}},
                                                          {identifier, #{line => 7,
                                                                         spec => i}}],
                                                line => 3,
                                                params => [],
                                                return_type => {type, #{line => 3,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Broken'}}]},
             left => {identifier, #{line => 6,
                                    spec => a,
                                    type => {type, #{line => 4,
                                                     source => inferred,
                                                     spec => atom}}}},
             locals => #{a => {type, #{line => 4,
                                       source => inferred,
                                       spec => atom}},
                         i => {type, #{line => 5,
                                       source => inferred,
                                       spec => int}}},
             right => {identifier, #{line => 6,
                                     spec => i,
                                     type => {type, #{line => 5,
                                                      source => inferred,
                                                      spec => int}}}}},
    ?assertEqual({error, unmatched_types, Data}, rufus_expr:typecheck_and_annotate(Forms)).
