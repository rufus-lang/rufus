-module(rufus_form_test).

-include_lib("eunit/include/eunit.hrl").

line_test() ->
    Form = {identifier, #{spec => n, line => 13}},
    ?assertEqual(13, rufus_form:line(Form)).

return_type_test() ->
    ReturnType = rufus_form:make_type(string, 13),
    Form = rufus_form:make_func('Ping', [], ReturnType, [], 13),
    ?assertEqual(ReturnType, rufus_form:return_type(Form)).

source_test() ->
    Type = rufus_form:make_type(int, 19),
    ?assertEqual(rufus_text, rufus_form:source(Type)),
    InferredType = rufus_form:make_inferred_type(int, 27),
    ?assertEqual(inferred, rufus_form:source(InferredType)).

spec_test() ->
    Form = {identifier, #{spec => n, line => 13}},
    ?assertEqual(n, rufus_form:spec(Form)).

has_type_test() ->
    Type = rufus_form:make_inferred_type(int, 27),
    Form = {int_lit, #{spec => 42, type => Type, line => 7}},
    ?assertEqual(true, rufus_form:has_type(Form)).

has_type_infers_identifier_type_from_locals_test() ->
    Type = rufus_form:make_inferred_type(int, 27),
    {FormType, Context} = rufus_form:make_identifier(number, 13),
    Locals = #{number => Type},
    Form = {FormType, Context#{locals => Locals}},
    ?assertEqual(true, rufus_form:has_type(Form)).

has_type_without_identifier_type_test() ->
    Form = {identifier, #{spec => unknown, line => 7}},
    ?assertEqual(false, rufus_form:has_type(Form)).

element_type_test() ->
    ElementType = rufus_form:make_type(bool, 81),
    Type = rufus_form:make_type(list, ElementType, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form = rufus_form:make_literal(list, Type, [Value], 81),
    ?assertEqual(ElementType, rufus_form:element_type(Type)),
    ?assertEqual(ElementType, rufus_form:element_type(Form)).

type_test() ->
    Type = rufus_form:make_inferred_type(int, 27),
    Form = {int_lit, #{spec => 42, type => Type, line => 7}},
    ?assertEqual(Type, rufus_form:type(Form)).

type_spec_with_rufus_form_test() ->
    Type = rufus_form:make_inferred_type(int, 27),
    Form = {int_lit, #{spec => 42, type => Type, line => 7}},
    ?assertEqual(int, rufus_form:type_spec(Form)).

type_spec_with_type_form_test() ->
    Type = rufus_form:make_inferred_type(int, 27),
    ?assertEqual(int, rufus_form:type_spec(Type)).

type_spec_with_locals_test() ->
    Locals = #{t => rufus_form:make_type(string, 3)},
    Form = {identifier, #{line => 3, locals => Locals, spec => t}},
    ?assertEqual(string, rufus_form:type_spec(Form)).

type_spec_with_unknown_form_test() ->
    Locals = #{other => rufus_form:make_type(string, 3)},
    Form = {identifier, #{line => 3, locals => Locals, spec => unknown}},
    ?assertEqual({error, unknown_form, #{form => Form}}, rufus_form:type_spec(Form)).

make_module_test() ->
    ?assertEqual({module, #{spec => example, line => 1}}, rufus_form:make_module(example, 1)).

make_import_test() ->
    ?assertEqual({import, #{spec => "example", line => 3}}, rufus_form:make_import("example", 3)).

make_identifier_test() ->
    ?assertEqual({identifier, #{spec => n, line => 13}}, rufus_form:make_identifier(n, 13)).

make_literal_test() ->
    ?assertEqual(
        {bool_lit, #{spec => true, line => 7, type => rufus_form:make_inferred_type(bool, 7)}},
        rufus_form:make_literal(bool, true, 7)
    ),
    ?assertEqual(
        {float_lit, #{
            spec => "37.103",
            line => 92,
            type => rufus_form:make_inferred_type(float, 92)
        }},
        rufus_form:make_literal(float, "37.103", 92)
    ),
    ?assertEqual(
        {int_lit, #{spec => "42", line => 5, type => rufus_form:make_inferred_type(int, 5)}},
        rufus_form:make_literal(int, "42", 5)
    ),
    ?assertEqual(
        {string_lit, #{
            spec => <<"hello">>,
            line => 9,
            type => rufus_form:make_inferred_type(string, 9)
        }},
        rufus_form:make_literal(string, <<"hello">>, 9)
    ).

make_binary_op_test() ->
    Left = rufus_form:make_literal(int, 2, 13),
    Right = rufus_form:make_literal(int, 3, 13),
    Form = rufus_form:make_binary_op('+', Left, Right, 13),
    ?assertEqual({binary_op, #{op => '+', left => Left, right => Right, line => 13}}, Form).

make_call_test() ->
    Line = 3,
    Spec = 'Echo',
    Args = [rufus_form:make_literal(string, <<"hello">>, 3)],
    Expected = {call, #{spec => Spec, args => Args, line => Line}},
    ?assertEqual(Expected, rufus_form:make_call(Spec, Args, Line)).

make_cons_test() ->
    Line = 3,
    Type = rufus_form:make_type(list, rufus_form:make_type(int, Line), Line),
    Head = rufus_form:make_literal(int, 5, 3),
    Tail = [],
    Expected = {cons, #{type => Type, head => Head, tail => Tail, line => Line}},
    ?assertEqual(Expected, rufus_form:make_cons(Type, Head, Tail, Line)).

make_func_test() ->
    Type = rufus_form:make_type(bool, 81),
    Param = rufus_form:make_literal(bool, true, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    ?assertEqual(
        {func, #{
            spec => 'True',
            params => [Param],
            return_type => Type,
            exprs => [Value],
            line => 81
        }},
        rufus_form:make_func('True', [Param], Type, [Value], 81)
    ).

make_param_test() ->
    Type = rufus_form:make_type(int, 52),
    ?assertEqual(
        {param, #{spec => n, type => Type, line => 52}},
        rufus_form:make_param(n, Type, 52)
    ).

make_inferred_type_test() ->
    ?assertEqual(
        {type, #{spec => int, source => inferred, line => 4}},
        rufus_form:make_inferred_type(int, 4)
    ).

make_type_test() ->
    ?assertEqual(
        {type, #{spec => float, source => rufus_text, line => 37}},
        rufus_form:make_type(float, 37)
    ).

make_match_test() ->
    Left = rufus_form:make_identifier(n, 3),
    Right = rufus_form:make_identifier(m, 3),
    ?assertEqual(
        {match, #{left => Left, right => Right, line => 3}},
        rufus_form:make_match(Left, Right, 3)
    ).
