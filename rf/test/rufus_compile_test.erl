-module(rufus_compile_test).

-include_lib("eunit/include/eunit.hrl").

eval_chain_without_handlers_test() ->
    ?assertEqual({ok, "hello!"}, rufus_compile:eval_chain("hello!", [])).

eval_chain_with_ok_handler_test() ->
    Reverse = fun(Word) -> {ok, lists:reverse(Word)} end,
    ?assertEqual({ok, "ahah"}, rufus_compile:eval_chain("haha", [Reverse])).

eval_chain_with_error_handler_test() ->
    Error = fun(_) -> {error, reason} end,
    Explode = fun(_) -> throw(explode) end,
    %% Explode never runs because eval_chain stops iterating over handlers when
    %% it encounters a failure from Error.
    ?assertEqual({error, reason}, rufus_compile:eval_chain("haha", [Error, Explode])).
