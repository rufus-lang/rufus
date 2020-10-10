-module(rufus_forms_test).

-include_lib("eunit/include/eunit.hrl").

%% Enumeration API

map_with_empty_input_test() ->
    ?assertEqual([], rufus_forms:map([], fun annotate/1)).

map_with_noop_function_test() ->
    Form = rufus_form:make_literal(bool, true, 7),
    ?assertEqual([Form], rufus_forms:map([Form], fun(F) -> F end)).

map_test() ->
    Form = rufus_form:make_literal(bool, true, 7),
    Expected1 =
        {bool_lit,
            Context = #{spec => true, line => 7, type => rufus_form:make_inferred_type(bool, 7)}},
    ?assertEqual(Expected1, Form),
    [AnnotatedForm] = rufus_forms:map([Form], fun annotate/1),
    Expected2 = {bool_lit, Context#{annotated => true}},
    ?assertEqual(Expected2, AnnotatedForm).

map_with_binary_op_test() ->
    Left = rufus_form:make_literal(int, 2, 13),
    Right = rufus_form:make_literal(int, 3, 13),
    Form = rufus_form:make_binary_op('+', Left, Right, 13),
    ?assertMatch(
        [
            {binary_op, #{
                left := {_, #{annotated := true}},
                right := {_, #{annotated := true}},
                annotated := true
            }}
        ],
        rufus_forms:map([Form], fun annotate/1)
    ).

map_with_call_test() ->
    Args = [rufus_form:make_literal(string, <<"hello">>, 3)],
    Form = rufus_form:make_call('Echo', Args, 13),
    ?assertMatch(
        [{call, #{args := [{_, #{annotated := true}}], annotated := true}}],
        rufus_forms:map([Form], fun annotate/1)
    ).

map_with_cons_test() ->
    Line = 17,
    Type = rufus_form:make_type(list, rufus_form:make_type(int, Line), Line),
    TailForm = rufus_form:make_cons(Type, rufus_form:make_literal(int, 3, Line), [], Line),
    Form = rufus_form:make_cons(Type, rufus_form:make_literal(int, 3, Line), [TailForm], Line),
    ?assertMatch(
        [
            {cons, #{
                head := {_, #{annotated := true}},
                tail := [
                    {cons, #{
                        head := {_, #{annotated := true}},
                        tail := [],
                        annotated := true
                    }}
                ],
                annotated := true
            }}
        ],
        rufus_forms:map([Form], fun annotate/1)
    ).

map_with_cons_and_tail_identifier_test() ->
    Line = 17,
    Type = rufus_form:make_type(list, rufus_form:make_type(int, Line), Line),
    TailForm = rufus_form:make_identifier('tail', 3),
    Form = rufus_form:make_cons(Type, rufus_form:make_literal(int, 3, Line), TailForm, Line),
    ?assertMatch(
        [
            {cons, #{
                head := {_, #{annotated := true}},
                tail := {identifier, #{spec := tail}},
                annotated := true
            }}
        ],
        rufus_forms:map([Form], fun annotate/1)
    ).

map_with_func_test() ->
    Type = rufus_form:make_type(bool, 81),
    Param = rufus_form:make_literal(bool, true, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form = rufus_form:make_func('True', [Param], Type, [Value], 81),
    ?assertMatch(
        [
            {func, #{
                params := [{bool_lit, #{annotated := true}}],
                exprs := [{bool_lit, #{annotated := true}}],
                annotated := true
            }}
        ],
        rufus_forms:map([Form], fun annotate/1)
    ).

map_with_list_lit_test() ->
    ElementType = rufus_form:make_type(bool, 81),
    Type = rufus_form:make_type(list, ElementType, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form = rufus_form:make_literal(list, Type, [Value], 81),
    ?assertMatch(
        [
            {list_lit, #{
                elements := [{bool_lit, #{annotated := true}}],
                annotated := true
            }}
        ],
        rufus_forms:map([Form], fun annotate/1)
    ).

map_with_match_test() ->
    Left = rufus_form:make_identifier(n, 3),
    Right = rufus_form:make_identifier(m, 3),
    Form = rufus_form:make_match(Left, Right, 3),
    ?assertMatch(
        [
            {match, #{
                left := {_, #{annotated := true}},
                right := {_, #{annotated := true}},
                annotated := true
            }}
        ],
        rufus_forms:map([Form], fun annotate/1)
    ).

annotate({FormType, Context}) ->
    {FormType, Context#{annotated => true}}.

%% Scope API

globals_without_func_forms_test() ->
    RufusText = "module empty",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, #{}}, rufus_forms:globals(Forms)).

globals_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n string) string { n }\n"
        "    func Number() int { 42 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = #{
        'Echo' => [
            {type, #{
                kind => func,
                line => 3,
                param_types => [{type, #{line => 3, spec => string}}],
                return_type =>
                    {type, #{line => 3, spec => string}},
                source => rufus_text,
                spec => 'func(string) string'
            }}
        ],
        'Number' => [
            {type, #{
                kind => func,
                line => 4,
                param_types => [],
                return_type =>
                    {type, #{line => 4, spec => int}},
                source => rufus_text,
                spec => 'func() int'
            }}
        ]
    },
    ?assertEqual({ok, Expected}, rufus_forms:globals(AnnotatedForms)).

globals_with_multiple_function_heads_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(n string) string { n }\n"
        "    func Echo(n int) int { n }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = #{
        'Echo' => [
            {type, #{
                kind => func,
                line => 3,
                param_types => [{type, #{line => 3, spec => string}}],
                return_type =>
                    {type, #{line => 3, spec => string}},
                source => rufus_text,
                spec => 'func(string) string'
            }},
            {type, #{
                kind => func,
                line => 4,
                param_types => [{type, #{line => 4, spec => int}}],
                return_type =>
                    {type, #{line => 4, spec => int}},
                source => rufus_text,
                spec => 'func(int) int'
            }}
        ]
    },
    ?assertEqual({ok, Expected}, rufus_forms:globals(AnnotatedForms)).
