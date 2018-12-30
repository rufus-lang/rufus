-module(rufus_compiler_test).

-include_lib("eunit/include/eunit.hrl").

eval_chain_without_handlers_test() ->
    ?assertEqual({ok, "hello!"}, rufus_compiler:eval_chain("hello!", [])).

eval_chain_with_ok_handler_test() ->
    Reverse = fun(Word) -> {ok, lists:reverse(Word)} end,
    ?assertEqual({ok, "ahah"}, rufus_compiler:eval_chain("haha", [Reverse])).

eval_chain_with_error_handler_test() ->
    Error = fun(_) -> {error, reason} end,
    Explode = fun(_) -> throw(explode) end,
    %% Explode never runs because eval_chain stops iterating over handlers when
    %% it encounters an error.
    ?assertEqual({error, reason}, rufus_compiler:eval_chain("haha", [Error, Explode])).

eval_with_function_returning_a_float_test() ->
    RufusText = "
    package example
    func Pi() float { 3.14159265359 }
    ",
    {ok, example} = rufus_compiler:eval(RufusText),
    ?assertEqual({float, 3.14159265359}, example:'Pi'()).

eval_with_function_returning_an_int_test() ->
    RufusText = "
    package example
    func Number() int { 42 }
    ",
    {ok, example} = rufus_compiler:eval(RufusText),
    ?assertEqual({int, 42}, example:'Number'()).

eval_with_function_returning_a_string_test() ->
    RufusText = "
    package example
    func Greeting() string { \"Hello\" }
    ",
    {ok, example} = rufus_compiler:eval(RufusText),
    ?assertEqual({string, <<"Hello">>}, example:'Greeting'()).

eval_with_function_having_unmatched_return_types_test() ->
    RufusText = "
    package example
    func Number() float { 42 }
    ",
    {error, unmatched_return_type, _Data} = rufus_compiler:eval(RufusText).
