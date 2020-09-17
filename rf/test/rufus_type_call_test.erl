-module(rufus_type_call_test).

-include_lib("eunit/include/eunit.hrl").

resolve_call_with_no_arguments_test() ->
    RufusText = "
    module example
    func Random() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Random', [], 7),
    Expected = rufus_form:make_type(int, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_call_with_one_argument_test() ->
    RufusText = "
    module example
    func Echo(text string) string { text }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7),
    Expected = rufus_form:make_type(string, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_call_with_one_argument_and_many_function_heads_test() ->
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
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7),
    Expected = rufus_form:make_type(string, 7),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_call_with_two_argument_test() ->
    RufusText = "
    module example
    func Concatenate(a atom, b string) string { \"hello world\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Concatenate', [rufus_form:make_literal(atom, hello, 7),
                                                rufus_form:make_literal(string, <<"world">>, 7)], 7),
    Expected = rufus_form:make_type(string, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_unknown_func_error_test() ->
    Form = rufus_form:make_call('Ping', [], 7),
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
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Ping', [], 7),
    Data = #{args => [],
             funcs => [{func, #{params => {params, #{line => 3,
                                                     params => [{param, #{line => 3,
                                                                          spec => message,
                                                                          type => {type, #{line => 3,
                                                                                           source => rufus_text,
                                                                                           spec => string}}}}]}},
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
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Echo', [rufus_form:make_literal(integer, 42, 7)], 7),
    Data = #{args => [{integer_lit, #{line => 7,
                                      spec => 42,
                                      type => {type, #{line => 7,
                                                       source => inferred,
                                                       spec => integer}}}}],
             funcs => [{func, #{params => {params, #{line => 3,
                                                     params => [{param, #{line => 3,
                                                                          spec => text,
                                                                          type => {type, #{line => 3,
                                                                                           source => rufus_text,
                                                                                           spec => string}}}}]}},
                                exprs => [{identifier, #{line => 3,
                                                         spec => text}}],
                                line => 3,
                                return_type => {type, #{line => 3,
                                                        source => rufus_text,
                                                        spec => string}},
                                spec => 'Echo'}}]},
    ?assertEqual({error, unmatched_args, Data}, rufus_type:resolve(Globals, Form)).
