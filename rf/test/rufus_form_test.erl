-module(rufus_form_test).

-include_lib("eunit/include/eunit.hrl").

line_test() ->
    Form = {identifier, #{spec => n, line => 13}},
    ?assertEqual(13, rufus_form:line(Form)).

source_test() ->
    Type = rufus_form:make_type(int, 19),
    ?assertEqual(rufus_text, rufus_form:source(Type)),
    InferredType = rufus_form:make_inferred_type(int, 27),
    ?assertEqual(inferred, rufus_form:source(InferredType)).

spec_test() ->
    Form = {identifier, #{spec => n, line => 13}},
    ?assertEqual(n, rufus_form:spec(Form)).

type_test() ->
    Type = rufus_form:make_inferred_type(int, 27),
    Form = {int_lit, #{spec => 42, type => Type, line => 7}},
    ?assertEqual(Type, rufus_form:type(Form)).

type_spec_test() ->
    Type = rufus_form:make_inferred_type(int, 27),
    Form = {int_lit, #{spec => 42, type => Type, line => 7}},
    ?assertEqual(int, rufus_form:type_spec(Form)).

make_module_test() ->
    ?assertEqual({module, #{spec => example, line => 1}}, rufus_form:make_module(example, 1)).

make_import_test() ->
    ?assertEqual({import, #{spec => "example", line => 3}}, rufus_form:make_import("example", 3)).

make_identifier_test() ->
    ?assertEqual({identifier, #{spec => n, line => 13}}, rufus_form:make_identifier(n, 13)).

make_literal_test() ->
    ?assertEqual({bool_lit, #{spec => true, line => 7, type => rufus_form:make_inferred_type(bool, 7)}},
                 rufus_form:make_literal(bool, true, 7)),
    ?assertEqual({float_lit, #{spec => "37.103", line => 92, type => rufus_form:make_inferred_type(float, 92)}},
                 rufus_form:make_literal(float, "37.103", 92)),
    ?assertEqual({int_lit, #{spec => "42", line => 5, type => rufus_form:make_inferred_type(int, 5)}},
                 rufus_form:make_literal(int, "42", 5)),
    ?assertEqual({string_lit, #{spec => <<"hello">>, line => 9, type => rufus_form:make_inferred_type(string, 9)}},
                 rufus_form:make_literal(string, <<"hello">>, 9)).

make_binary_op_test() ->
    Arg = {int_lit, #{spec => 2, line => 4}},
    ?assertEqual({binary_op, #{op => '+', left => Arg, right => Arg, line => 4}},
                 rufus_form:make_binary_op('+', Arg, Arg, 4)).

make_func_decl_test() ->
    Type = rufus_form:make_type(bool, 81),
    ?assertEqual({func_decl, #{spec => 'True', args => [], return_type => Type, exprs => [], line => 81}},
                 rufus_form:make_func_decl('True', [], Type, [], 81)).

make_arg_test() ->
    Type = rufus_form:make_type(int, 52),
    ?assertEqual({arg, #{spec => n, type => Type, line => 52}}, rufus_form:make_arg(n, Type, 52)).

make_inferred_type_test() ->
    ?assertEqual({type, #{spec => int, source => inferred, line => 4}}, rufus_form:make_inferred_type(int, 4)).

make_type_test() ->
    ?assertEqual({type, #{spec => float, source => rufus_text, line => 37}}, rufus_form:make_type(float, 37)).
