-module(rufus_type_call_test).

-include_lib("eunit/include/eunit.hrl").

%% resolve type for a call form

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
             funcs => [{func, #{params => [{param, #{line => 3,
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
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Echo', [rufus_form:make_literal(integer, 42, 7)], 7),
    Data = #{args => [{integer_lit, #{line => 7,
                                      spec => 42,
                                      type => {type, #{line => 7,
                                                       source => inferred,
                                                       spec => integer}}}}],
             funcs => [{func, #{params => [{param, #{line => 3,
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
