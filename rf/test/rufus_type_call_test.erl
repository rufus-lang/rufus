-module(rufus_type_call_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("rufus_type.hrl").

resolve_call_with_no_arguments_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Random() int { 42 }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, Globals} = rufus_forms:globals(AnnotatedForms),
    {ok, Form} = annotate_locals(
        #{},
        rufus_form:make_call('Random', [], 7)
    ),
    Expected = rufus_form:make_type(int, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_call_with_one_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(text string) string { text }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, Globals} = rufus_forms:globals(AnnotatedForms),
    {ok, Form} = annotate_locals(
        #{},
        rufus_form:make_call('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7)
    ),
    Expected = rufus_form:make_type(string, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_call_with_one_argument_and_many_function_heads_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(name atom) atom { name }\n"
        "    func Echo(b bool) bool { b }\n"
        "    func Echo(n float) float { n }\n"
        "    func Echo(n int) int { n }\n"
        "    func Echo(text string) string { text }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, Globals} = rufus_forms:globals(AnnotatedForms),
    {ok, Form} = annotate_locals(
        #{},
        rufus_form:make_call('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7)
    ),
    Expected = rufus_form:make_type(string, 7),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_call_with_two_argument_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Concatenate(a atom, b string) string { \"hello world\" }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, Globals} = rufus_forms:globals(AnnotatedForms),
    {ok, Form} = annotate_locals(
        #{},
        rufus_form:make_call(
            'Concatenate',
            [
                rufus_form:make_literal(atom, hello, 7),
                rufus_form:make_literal(string, <<"world">>, 7)
            ],
            7
        )
    ),
    Expected = rufus_form:make_type(string, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_unknown_func_error_test() ->
    {ok, Form} = annotate_locals(
        #{},
        rufus_form:make_call('Ping', [], 7)
    ),
    Data = #{
        form =>
            {call, #{
                args => [],
                line => 7,
                locals => #{},
                spec => 'Ping'
            }},
        globals => #{},
        stack => []
    },
    ?assertEqual({error, unknown_func, Data}, rufus_type:resolve(#{}, Form)).

resolve_unknown_arity_error_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Ping(message string) string { message }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, Globals} = rufus_forms:globals(AnnotatedForms),
    {ok, Form} = annotate_locals(
        #{},
        rufus_form:make_call('Ping', [], 7)
    ),
    Data = #{
        args => [],
        types => [
            {type, #{
                kind => func,
                line => 3,
                param_types => [{type, #{line => 3, spec => string}}],
                return_type =>
                    {type, #{line => 3, spec => string}},
                source => rufus_text,
                spec => 'func(string) string'
            }}
        ]
    },
    ?assertEqual({error, unknown_arity, Data}, rufus_type:resolve(Globals, Form)).

resolve_unmatched_args_error_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(text string) string { text }\n"
        "    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, Globals} = rufus_forms:globals(AnnotatedForms),
    {ok, Form} = annotate_locals(
        #{},
        rufus_form:make_call('Echo', [rufus_form:make_literal(integer, 42, 7)], 7)
    ),
    Data = #{
        args => [
            {integer_lit, #{
                line => 7,
                spec => 42,
                type =>
                    {type, #{line => 7, spec => integer}}
            }}
        ],
        types => [
            {type, #{
                kind => func,
                line => 3,
                param_types => [{type, #{line => 3, spec => string}}],
                return_type =>
                    {type, #{line => 3, spec => string}},
                source => rufus_text,
                spec => 'func(string) string'
            }}
        ]
    },
    ?assertEqual({error, unmatched_args, Data}, rufus_type:resolve(Globals, Form)).

-spec annotate_locals(locals(), rufus_form()) -> {ok, rufus_form()}.
annotate_locals(Locals, {FormType, Context}) ->
    {ok, {FormType, Context#{locals => Locals}}}.
