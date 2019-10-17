-module(rufus_type_test).

-include_lib("eunit/include/eunit.hrl").

resolve_form_with_an_existing_type_form_test() ->
    Form = rufus_form:make_literal(bool, true, 7),
    Expected = {type, #{spec => bool, source => inferred, line => 7}},
    ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form)).

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
             form_decls => [{func_decl, #{args => [{arg_decl, #{line => 3,
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
    Expected = {error, unknown_arity, Data},
    ?assertEqual(Expected, rufus_type:resolve(Globals, Form)).
