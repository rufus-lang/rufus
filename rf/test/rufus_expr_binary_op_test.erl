-module(rufus_expr_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

%% arithmetic binary_op expressions

typecheck_and_annotate_arithmetic_binary_op_with_ints_test() ->
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

typecheck_and_annotate_arithmetic_binary_op_with_floats_test() ->
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

typecheck_and_annotate_arithmetic_binary_op_with_float_and_int_test() ->
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

typecheck_and_annotate_arithmetic_binary_op_with_float_and_float_and_int_test() ->
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

typecheck_and_annotate_arithmetic_binary_op_with_bools_test() ->
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

typecheck_and_annotate_arithmetic_binary_op_with_strings_test() ->
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

%% arithmetic binary_op expressions with the remainder operator

typecheck_and_annotate_remainder_arithmetic_binary_op_with_ints_test() ->
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

typecheck_and_annotate_remainder_arithmetic_binary_op_with_floats_test() ->
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
    {error, unsupported_operand_type, Data} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, Data).

%% boolean binary_op expressions

typecheck_and_annotate_boolean_binary_op_with_bools_test() ->
    RufusText = "
    module example
    func Falsy() bool { true and false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{params => [],
                         exprs => [{binary_op, #{left => {bool_lit, #{line => 3,
                                                                      spec => true,
                                                                      type => {type, #{line => 3,
                                                                                       source => inferred,
                                                                                       spec => bool}}}},
                                                 line => 3,
                                                 op => 'and',
                                                 right => {bool_lit, #{line => 3,
                                                                       spec => false,
                                                                       type => {type, #{line => 3,
                                                                                        source => inferred,
                                                                                        spec => bool}}}},
                                                 type => {type, #{line => 3,
                                                                  source => inferred,
                                                                  spec => bool}}}}],
                         line => 3,
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => bool}},
                         spec => 'Falsy'}}],
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_boolean_binary_op_with_bool_and_int_test() ->
    RufusText = "
    module example
    func Falsy() bool { true and 0 }
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
                                       op => 'and',
                                       right => {int_lit, #{line => 3,
                                                            spec => 0,
                                                            type => {type, #{line => 3,
                                                                             source => inferred,
                                                                             spec => int}}}}}}},
    {error, unsupported_operand_type, Data} = rufus_expr:typecheck_and_annotate(Forms),
    ?assertEqual(Expected, Data).
