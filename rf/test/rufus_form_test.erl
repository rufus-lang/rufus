-module(rufus_form_test).

-include_lib("eunit/include/eunit.hrl").

%% Form API

line_test() ->
    Form =
        {identifier, #{
            spec => n,
            line => 13
        }},
    ?assertEqual(13, rufus_form:line(Form)).

return_type_test() ->
    ReturnType = rufus_form:make_type(string, 13),
    Form = rufus_form:make_func('Ping', [], ReturnType, [], 13),
    ?assertEqual(ReturnType, rufus_form:return_type(Form)).

spec_test() ->
    Form =
        {identifier, #{
            spec => n,
            line => 13
        }},
    ?assertEqual(n, rufus_form:spec(Form)).

has_type_test() ->
    Type = rufus_form:make_type(int, 27),
    Form =
        {int_lit, #{
            spec => 42,
            type => Type,
            line => 7
        }},
    ?assertEqual(true, rufus_form:has_type(Form)).

has_type_infers_identifier_type_from_locals_test() ->
    Type = rufus_form:make_type(int, 27),
    {FormType, Context} = rufus_form:make_identifier(number, 13),
    Locals = #{number => [Type]},
    Form = {FormType, Context#{locals => Locals}},
    ?assertEqual(true, rufus_form:has_type(Form)).

has_type_without_identifier_type_test() ->
    Form =
        {identifier, #{
            spec => unknown,
            line => 7
        }},
    ?assertEqual(false, rufus_form:has_type(Form)).

element_type_test() ->
    ElementType = rufus_form:make_type(bool, 81),
    Type = rufus_form:make_type(list, ElementType, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form = rufus_form:make_literal(list, Type, [Value], 81),
    ?assertEqual(ElementType, rufus_form:element_type(Type)),
    ?assertEqual(ElementType, rufus_form:element_type(Form)).

type_test() ->
    Type = rufus_form:make_type(int, 27),
    Form =
        {int_lit, #{
            spec => 42,
            type => Type,
            line => 7
        }},
    ?assertEqual(Type, rufus_form:type(Form)).

type_spec_with_rufus_form_test() ->
    Type = rufus_form:make_type(int, 27),
    Form =
        {int_lit, #{
            spec => 42,
            type => Type,
            line => 7
        }},
    ?assertEqual(int, rufus_form:type_spec(Form)).

type_spec_with_type_form_test() ->
    Type = rufus_form:make_type(int, 27),
    ?assertEqual(int, rufus_form:type_spec(Type)).

type_spec_with_locals_test() ->
    Locals = #{t => [rufus_form:make_type(string, 3)]},
    Form =
        {identifier, #{
            line => 3,
            locals => Locals,
            spec => t
        }},
    ?assertEqual(string, rufus_form:type_spec(Form)).

type_spec_with_unknown_form_test() ->
    Locals = #{other => [rufus_form:make_type(string, 3)]},
    Form =
        {identifier, #{
            line => 3,
            locals => Locals,
            spec => unknown
        }},
    ?assertEqual({error, unknown_form, #{form => Form}}, rufus_form:type_spec(Form)).

%% Form builder API

make_binary_op_test() ->
    Left = rufus_form:make_literal(int, 2, 13),
    Right = rufus_form:make_literal(int, 3, 13),
    Form = rufus_form:make_binary_op('+', Left, Right, 13),
    Expected =
        {binary_op, #{
            op => '+',
            left => Left,
            right => Right,
            line => 13
        }},
    ?assertEqual(Expected, Form).

make_call_test() ->
    Line = 3,
    Spec = 'Echo',
    Args = [rufus_form:make_literal(string, <<"hello">>, 3)],
    Expected =
        {call, #{
            spec => Spec,
            args => Args,
            line => Line
        }},
    ?assertEqual(Expected, rufus_form:make_call(Spec, Args, Line)).

make_cons_test() ->
    Line = 3,
    Type = rufus_form:make_type(list, rufus_form:make_type(int, Line), Line),
    Head = rufus_form:make_literal(int, 5, 3),
    Tail = [],
    Expected =
        {cons, #{
            type => Type,
            head => Head,
            tail => Tail,
            line => Line
        }},
    ?assertEqual(Expected, rufus_form:make_cons(Type, Head, Tail, Line)).

make_cons_head_test() ->
    Line = 3,
    Type = rufus_form:make_type(list, rufus_form:make_type(int, Line), Line),
    Head = rufus_form:make_literal(int, 5, 3),
    Tail = [],
    Form = rufus_form:make_cons(Type, Head, Tail, Line),
    Expected =
        {cons_head, #{
            line => 3
        }},
    ?assertEqual(Expected, rufus_form:make_cons_head(Form)).

make_cons_tail_test() ->
    Line = 3,
    Type = rufus_form:make_type(list, rufus_form:make_type(int, Line), Line),
    Head = rufus_form:make_literal(int, 5, 3),
    Tail = [],
    Form = rufus_form:make_cons(Type, Head, Tail, Line),
    Expected =
        {cons_tail, #{
            line => 3
        }},
    ?assertEqual(Expected, rufus_form:make_cons_tail(Form)).

make_func_test() ->
    Type = rufus_form:make_type(bool, 81),
    Param = rufus_form:make_literal(bool, true, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Expected =
        {func, #{
            spec => 'True',
            params => [Param],
            return_type => Type,
            exprs => [Value],
            line => 81
        }},
    ?assertEqual(Expected, rufus_form:make_func('True', [Param], Type, [Value], 81)).

make_func_exprs_test() ->
    Type = rufus_form:make_type(bool, 81),
    Param = rufus_form:make_literal(bool, true, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form =
        {func, #{
            spec => 'True',
            params => [Param],
            return_type => Type,
            exprs => [Value],
            line => 81
        }},
    Expected =
        {func_exprs, #{
            line => 81
        }},
    ?assertEqual(Expected, rufus_form:make_func_exprs(Form)).

make_func_params_test() ->
    Type = rufus_form:make_type(bool, 81),
    Param = rufus_form:make_literal(bool, true, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form =
        {func, #{
            spec => 'True',
            params => [Param],
            return_type => Type,
            exprs => [Value],
            line => 81
        }},
    Expected =
        {func_params, #{
            line => 81
        }},
    ?assertEqual(Expected, rufus_form:make_func_params(Form)).

make_param_test() ->
    Type = rufus_form:make_type(int, 52),
    Expected =
        {param, #{
            spec => n,
            type => Type,
            line => 52
        }},
    ?assertEqual(Expected, rufus_form:make_param(n, Type, 52)).

make_identifier_test() ->
    Expected =
        {identifier, #{
            spec => n,
            line => 13
        }},
    ?assertEqual(Expected, rufus_form:make_identifier(n, 13)).

make_import_test() ->
    Expected =
        {import, #{
            spec => "example",
            line => 3
        }},
    ?assertEqual(Expected, rufus_form:make_import("example", 3)).

make_literal_for_bool_lit_test() ->
    Expected =
        {bool_lit, #{
            spec => true,
            line => 7,
            type => rufus_form:make_type(bool, 7)
        }},
    ?assertEqual(Expected, rufus_form:make_literal(bool, true, 7)).

make_literal_for_float_lit_test() ->
    Expected =
        {float_lit, #{
            spec => "37.103",
            line => 92,
            type => rufus_form:make_type(float, 92)
        }},
    ?assertEqual(Expected, rufus_form:make_literal(float, "37.103", 92)).

make_literal_for_int_lit_test() ->
    Expected =
        {int_lit, #{
            spec => "42",
            line => 5,
            type => rufus_form:make_type(int, 5)
        }},
    ?assertEqual(Expected, rufus_form:make_literal(int, "42", 5)).

make_literal_for_string_lit_test() ->
    Expected =
        {string_lit, #{
            spec => <<"hello">>,
            line => 9,
            type => rufus_form:make_type(string, 9)
        }},
    ?assertEqual(Expected, rufus_form:make_literal(string, <<"hello">>, 9)).

make_match_op_test() ->
    Left = rufus_form:make_identifier(n, 3),
    Right = rufus_form:make_identifier(m, 3),
    Expected =
        {match_op, #{
            left => Left,
            right => Right,
            line => 3
        }},
    ?assertEqual(Expected, rufus_form:make_match_op(Left, Right, 3)).

make_match_op_left_test() ->
    Left = rufus_form:make_identifier(n, 3),
    Right = rufus_form:make_identifier(m, 3),
    Form =
        {match_op, #{
            left => Left,
            right => Right,
            line => 3
        }},
    Expected =
        {match_op_left, #{
            line => 3
        }},
    ?assertEqual(Expected, rufus_form:make_match_op_left(Form)).

make_match_op_right_test() ->
    Left = rufus_form:make_identifier(n, 3),
    Right = rufus_form:make_identifier(m, 3),
    Form =
        {match_op, #{
            left => Left,
            right => Right,
            line => 3
        }},
    Expected =
        {match_op_right, #{
            line => 3
        }},
    ?assertEqual(Expected, rufus_form:make_match_op_right(Form)).

make_module_test() ->
    Expected =
        {module, #{
            spec => example,
            line => 1
        }},
    ?assertEqual(Expected, rufus_form:make_module(example, 1)).

make_throw_test() ->
    Expr = rufus_form:make_literal(bool, true, 7),
    ?assertEqual(
        {throw, #{expr => Expr, line => 7}},
        rufus_form:make_throw(Expr, 7)
    ).

make_type_test() ->
    ?assertEqual(
        {type, #{spec => int, line => 4}},
        rufus_form:make_type(int, 4)
    ).
