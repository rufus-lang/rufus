-module(rufus_form_test).

-include_lib("eunit/include/eunit.hrl").

globals_without_func_forms_test() ->
    RufusText = "module empty",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, #{}}, rufus_form:globals(Forms)).

globals_test() ->
    RufusText = "
    module example
    func Echo(n string) string { n }
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Echo1 = {func, #{params => {params, #{params => [{param, #{line => 3,
                                                               spec => n,
                                                               type => {type, #{line => 3,
                                                                                source => rufus_text,
                                                                                spec => string}}}}],
                                          line => 3}},
                     exprs => [{identifier, #{line => 3,
                                              spec => n}}],
                     line => 3,
                     return_type => {type, #{line => 3,
                                             source => rufus_text,
                                             spec => string}},
                     spec => 'Echo'}},
    Number0 = {func, #{params => {params, #{params => [],
                                            line => 4}},
                       exprs => [{int_lit, #{line => 4,
                                             spec => 42,
                                             type => {type, #{line => 4,
                                                              source => inferred,
                                                              spec => int}}}}],
                       line => 4,
                       return_type => {type, #{line => 4,
                                               source => rufus_text,
                                               spec => int}},
                       spec => 'Number'}},
    ?assertEqual({ok, #{'Echo' => [Echo1], 'Number' => [Number0]}}, rufus_form:globals(Forms)).

globals_with_multiple_function_heads_test() ->
    RufusText = "
    module example
    func Echo(n string) string { n }
    func Echo(n int) int { n }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    EchoString = {func, #{params => {params, #{params => [{param, #{line => 3,
                                                                    spec => n,
                                                                    type => {type, #{line => 3,
                                                                                     source => rufus_text,
                                                                                     spec => string}}}}],
                                              line => 3}},
                          exprs => [{identifier, #{line => 3,
                                                   spec => n}}],
                          line => 3,
                          return_type => {type, #{line => 3,
                                                  source => rufus_text,
                                                  spec => string}},
                          spec => 'Echo'}},
    EchoInt = {func, #{params => {params, #{params => [{param, #{line => 4,
                                                                 spec => n,
                                                                 type => {type, #{line => 4,
                                                                                  source => rufus_text,
                                                                                  spec => int}}}}],
                                            line => 4}},
                       exprs => [{identifier, #{line => 4,
                                                spec => n}}],
                       line => 4,
                       return_type => {type, #{line => 4,
                                               source => rufus_text,
                                               spec => int}},
                       spec => 'Echo'}},
    ?assertEqual({ok, #{'Echo' => [EchoString, EchoInt]}}, rufus_form:globals(Forms)).

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
    ?assertEqual({bool_lit, #{spec => true, line => 7, type => rufus_form:make_inferred_type(bool, 7)}},
                 rufus_form:make_literal(bool, true, 7)),
    ?assertEqual({float_lit, #{spec => "37.103", line => 92, type => rufus_form:make_inferred_type(float, 92)}},
                 rufus_form:make_literal(float, "37.103", 92)),
    ?assertEqual({int_lit, #{spec => "42", line => 5, type => rufus_form:make_inferred_type(int, 5)}},
                 rufus_form:make_literal(int, "42", 5)),
    ?assertEqual({string_lit, #{spec => <<"hello">>, line => 9, type => rufus_form:make_inferred_type(string, 9)}},
                 rufus_form:make_literal(string, <<"hello">>, 9)).

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
    ?assertEqual({func, #{spec => 'True',
                          params => {params, #{params => [Param],
                                               line => 81}},
                          return_type => Type,
                          exprs => [Value],
                          line => 81}},
                 rufus_form:make_func('True', [Param], Type, [Value], 81)).

make_param_test() ->
    Type = rufus_form:make_type(int, 52),
    ?assertEqual({param, #{spec => n,
                           type => Type,
                           line => 52}},
                 rufus_form:make_param(n, Type, 52)).

make_inferred_type_test() ->
    ?assertEqual({type, #{spec => int,
                          source => inferred,
                          line => 4}},
                 rufus_form:make_inferred_type(int, 4)).

make_type_test() ->
    ?assertEqual({type, #{spec => float,
                          source => rufus_text,
                          line => 37}},
                 rufus_form:make_type(float, 37)).

make_match_test() ->
    Left = rufus_form:make_identifier(n, 3),
    Right = rufus_form:make_identifier(m, 3),
    ?assertEqual({match, #{left => Left,
                           right => Right,
                           line => 3}},
                 rufus_form:make_match(Left, Right, 3)).

map_with_empty_input_test() ->
    ?assertEqual([], rufus_form:map([], fun annotate/1)).

map_with_noop_function_test() ->
    Form = rufus_form:make_literal(bool, true, 7),
    ?assertEqual([Form], rufus_form:map([Form], fun(F) -> F end)).

map_test() ->
    Form = rufus_form:make_literal(bool, true, 7),
    Expected1 = {bool_lit, Context = #{spec => true, line => 7, type => rufus_form:make_inferred_type(bool, 7)}},
    ?assertEqual(Expected1, Form),
    [AnnotatedForm] = rufus_form:map([Form], fun annotate/1),
    Expected2 = {bool_lit, Context#{annotated => true}},
    ?assertEqual(Expected2, AnnotatedForm).

map_with_binary_op_test() ->
    Left = rufus_form:make_literal(int, 2, 13),
    Right = rufus_form:make_literal(int, 3, 13),
    Form = rufus_form:make_binary_op('+', Left, Right, 13),
    ?assertMatch([{binary_op, #{left := {_, #{annotated := true}}, right := {_, #{annotated := true}}, annotated := true}}],
                 rufus_form:map([Form], fun annotate/1)).

map_with_call_test() ->
    Args = [rufus_form:make_literal(string, <<"hello">>, 3)],
    Form = rufus_form:make_call('Echo', Args, 13),
    ?assertMatch([{call, #{args := [{_, #{annotated := true}}], annotated := true}}],
                 rufus_form:map([Form], fun annotate/1)).

map_with_cons_test() ->
    Line = 17,
    Type = rufus_form:make_type(list, rufus_form:make_type(int, Line), Line),
    TailForm = rufus_form:make_cons(Type, rufus_form:make_literal(int, 3, Line), [], Line),
    Form = rufus_form:make_cons(Type, rufus_form:make_literal(int, 3, Line), [TailForm], Line),
    ?assertMatch([{cons, #{head := {_, #{annotated := true}},
                           tail := [{cons, #{head := {_, #{annotated := true}},
                                             tail := [], annotated := true}}],
                           annotated := true}}],
                 rufus_form:map([Form], fun annotate/1)).

map_with_cons_and_tail_identifier_test() ->
    Line = 17,
    Type = rufus_form:make_type(list, rufus_form:make_type(int, Line), Line),
    TailForm = rufus_form:make_identifier('tail', 3),
    Form = rufus_form:make_cons(Type, rufus_form:make_literal(int, 3, Line), TailForm, Line),
    ?assertMatch([{cons, #{head := {_, #{annotated := true}},
                           tail := {identifier, #{spec := tail}},
                           annotated := true}}],
                 rufus_form:map([Form], fun annotate/1)).

map_with_func_test() ->
    Type = rufus_form:make_type(bool, 81),
    Param = rufus_form:make_literal(bool, true, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form = rufus_form:make_func('True', [Param], Type, [Value], 81),
    io:format("Form => ~p~n", [Form]),
    ?assertMatch([{func, #{params := {params, #{params := [{bool_lit, #{annotated := true}}]}},
                           exprs := [{bool_lit, #{annotated := true}}],
                           annotated := true}}],
                 rufus_form:map([Form], fun annotate/1)).

map_with_list_lit_test() ->
    ElementType = rufus_form:make_type(bool, 81),
    Type = rufus_form:make_type(list, ElementType, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form = rufus_form:make_literal(list, Type, [Value], 81),
    ?assertMatch([{list_lit, #{elements := [{bool_lit, #{annotated := true}}],
                               annotated := true}}],
                 rufus_form:map([Form], fun annotate/1)).

map_with_match_test() ->
    Left = rufus_form:make_identifier(n, 3),
    Right = rufus_form:make_identifier(m, 3),
    Form = rufus_form:make_match(Left, Right, 3),
    ?assertMatch([{match, #{left := {_, #{annotated := true}},
                            right := {_, #{annotated := true}},
                            annotated := true}}],
                 rufus_form:map([Form], fun annotate/1)).

annotate({FormType, Context}) ->
    {FormType, Context#{annotated => true}}.
