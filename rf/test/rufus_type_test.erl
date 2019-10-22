-module(rufus_type_test).

-include_lib("eunit/include/eunit.hrl").

%% resolve type for an apply form

resolve_unknown_func_error_test() ->
    Form = rufus_form:make_apply('Ping', [], 7),
    Expected = {error, unknown_func, #{args => [],
                                       spec => 'Ping'}},
    ?assertEqual(Expected, rufus_type:resolve(#{}, Form)).

resolve_unknown_arity_error_test() ->
    RufusText = "
    module example
    func Ping(message string) string { message }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Ping', [], 7),
    Data = #{arg_exprs => [],
             func_decls => [{func_decl, #{args => [{arg_decl, #{line => 3,
                                                                spec => message,
                                                                type => {type, #{line => 3,
                                                                                 source => rufus_text,
                                                                                 spec => string}}}}],
                                          exprs => [{identifier, #{line => 3,
                                                                   spec => message}}],
                                          line => 3,
                                          return_type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => string}},
                                          spec => 'Ping'}}]},
    ?assertEqual({error, unknown_arity, Data}, rufus_type:resolve(Globals, Form)).


resolve_unmatched_args_error_test() ->
    RufusText = "
    module example
    func Echo(text string) string { text }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Echo', [rufus_form:make_literal(integer, 42, 7)], 7),
    Data = #{arg_exprs => [{integer_lit, #{line => 7,
                                           spec => 42,
                                           type => {type, #{line => 7,
                                                            source => inferred,
                                                            spec => integer}}}}],
             func_decls => [{func_decl, #{args => [{arg_decl, #{line => 3,
                                                                spec => text,
                                                                type => {type, #{line => 3,
                                                                                 source => rufus_text,
                                                                                 spec => string}}}}],
                                          exprs => [{identifier, #{line => 3,
                                                                   spec => text}}],
                                          line => 3,
                                          return_type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => string}},
                                          spec => 'Echo'}}]},
    ?assertEqual({error, unmatched_args, Data}, rufus_type:resolve(Globals, Form)).

resolve_form_with_an_existing_type_form_test() ->
    Form = rufus_form:make_literal(bool, true, 7),
    Expected = {type, #{spec => bool, source => inferred, line => 7}},
    ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form)).

resolve_form_with_no_arguments_test() ->
    RufusText = "
    module example
    func Random() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Random', [], 7),
    Expected = rufus_form:make_type(int, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_form_with_one_argument_test() ->
    RufusText = "
    module example
    func Echo(text string) string { text }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7),
    Expected = rufus_form:make_type(string, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_form_with_one_argument_and_many_function_heads_test() ->
    RufusText = "
    module example
    func Echo(name atom) atom { name }
    func Echo(b bool) bool { b }
    func Echo(n float) float { n }
    func Echo(n int) int { n }
    func Echo(text string) string { text }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7),
    Expected = rufus_form:make_type(string, 7),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_form_with_two_argument_test() ->
    RufusText = "
    module example
    func Concatenate(a atom, b string) string { \"hello world\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Concatenate', [rufus_form:make_literal(atom, hello, 7),
                                                 rufus_form:make_literal(string, <<"world">>, 7)], 7),
    Expected = rufus_form:make_type(string, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

%% resolve type for a binary_op form

resolve_form_with_ints_test() ->
    Op = '+',
    Left = rufus_form:make_literal(int, 5, 9),
    Right = rufus_form:make_literal(int, 7, 9),
    Form = rufus_form:make_binary_op(Op, Left, Right, 9),
    Expected = rufus_form:make_inferred_type(int, 9),
    ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form)).

resolve_form_with_floats_test() ->
    Op = '+',
    Left = rufus_form:make_literal(float, 1.0, 9),
    Right = rufus_form:make_literal(float, 2.14159265359, 9),
    Form = rufus_form:make_binary_op(Op, Left, Right, 9),
    Expected = rufus_form:make_inferred_type(float, 9),
    ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form)).

resolve_unmatched_operand_type_error_test() ->
    Op = '+',
    Left = rufus_form:make_literal(int, 5, 9),
    Right = rufus_form:make_literal(float, 2.14159265359, 9),
    Form = rufus_form:make_binary_op(Op, Left, Right, 9),
    ?assertEqual({error, unmatched_operand_type, #{form => Form}}, rufus_type:resolve(#{}, Form)).

resolve_nested_unmatched_operand_type_error_test() ->
    Op = '+',
    Left = rufus_form:make_literal(float, 13.0, 9),
    Right = rufus_form:make_literal(float, 6.0, 9),
    LeftForm = rufus_form:make_binary_op(Op, Left, Right, 9),
    RightForm = rufus_form:make_literal(int, 23, 9),
    Form = rufus_form:make_binary_op(Op, LeftForm, RightForm, 9),
    ?assertEqual({error, unmatched_operand_type, #{form => Form}}, rufus_type:resolve(#{}, Form)).

resolve_unsupported_operand_type_error_test() ->
    Op = '+',
    Left = rufus_form:make_literal(bool, true, 9),
    Right = rufus_form:make_literal(bool, false, 9),
    Form = rufus_form:make_binary_op(Op, Left, Right, 9),
    ?assertEqual({error, unsupported_operand_type, #{form => Form}}, rufus_type:resolve(#{}, Form)).
