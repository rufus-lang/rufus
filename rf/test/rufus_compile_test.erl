-module(rufus_compile_test).

-include_lib("eunit/include/eunit.hrl").

eval_stages_without_handlers_test() ->
    ?assertEqual({ok, "hello!"}, rufus_compile:eval_stages("hello!", [])).

eval_stages_with_ok_handler_test() ->
    Reverse = fun(Word) -> {ok, lists:reverse(Word)} end,
    ?assertEqual({ok, "ahah"}, rufus_compile:eval_stages("haha", [Reverse])).

eval_stages_with_error_handler_test() ->
    Error = fun(_) -> {error, reason} end,
    Explode = fun(_) -> throw(explode) end,
    %% Explode never runs because eval_stages stops iterating over handlers when
    %% it encounters a failure from Error.
    ?assertEqual({error, reason}, rufus_compile:eval_stages("haha", [Error, Explode])).
