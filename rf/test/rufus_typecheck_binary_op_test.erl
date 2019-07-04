-module(rufus_typecheck_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

forms_typecheck_for_binary_op_with_ints_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 19 + 23 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, TypecheckedForms} = rufus_typecheck_binary_op:forms(Forms),
    ?assertEqual(Forms, TypecheckedForms).

forms_typecheck_for_binary_op_with_floats_test() ->
    RufusText = "
    module example
    func Pi() float { 1.0 + 2.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, TypecheckedForms} = rufus_typecheck_binary_op:forms(Forms),
    ?assertEqual(Forms, TypecheckedForms).

forms_typecheck_for_binary_op_with_float_and_int_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 19.0 + 23 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = #{op => '+',
                 left => {float_lit, #{line => 3,
                                       spec => 19.0,
                                       type => {type, #{line => 3, spec => float, source => inferred}}}},
                 right => {int_lit, #{line => 3,
                                      spec => 23,
                                      type => {type, #{line => 3, spec => int, source => inferred}}}}},
    {error, unmatched_operand_type, Data} = rufus_typecheck_binary_op:forms(Forms),
    ?assertEqual(Expected, Data).

forms_typecheck_for_binary_op_with_bools_test() ->
    RufusText = "
    module example
    func Concat() bool { true + false }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = #{op => '+',
                 left => {bool_lit, #{line => 3,
                                      spec => true,
                                      type => {type, #{line => 3, spec => bool, source => inferred}}}},
                 right => {bool_lit, #{line => 3,
                                       spec => false,
                                       type => {type, #{line => 3, spec => bool, source => inferred}}}}},
    {error, unsupported_operand_type, Data} = rufus_typecheck_binary_op:forms(Forms),
    ?assertEqual(Expected, Data).

forms_typecheck_for_binary_op_with_strings_test() ->
    RufusText = "
    module example
    func Concat() string { \"port\" + \"manteau\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = #{op => '+',
                 left => {string_lit, #{line => 3,
                                        spec => <<"port">>,
                                        type => {type, #{line => 3, spec => string, source => inferred}}}},
                 right => {string_lit, #{line => 3,
                                         spec => <<"manteau">>,
                                         type => {type, #{line => 3, spec => string, source => inferred}}}}},
    {error, unsupported_operand_type, Data} = rufus_typecheck_binary_op:forms(Forms),
    ?assertEqual(Expected, Data).
