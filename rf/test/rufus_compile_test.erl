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

%% Arity-0 functions returning a literal value for primitive types

eval_with_function_returning_a_bool_literal_test() ->
    RufusText = "
    package example
    func True() bool { true }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({bool, true}, example:'True'()).

eval_with_function_returning_a_float_literal_test() ->
    RufusText = "
    package example
    func Pi() float { 3.14159265359 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({float, 3.14159265359}, example:'Pi'()).

eval_with_function_returning_an_int_literal_test() ->
    RufusText = "
    package example
    func Number() int { 42 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({int, 42}, example:'Number'()).

eval_with_function_returning_a_string_literal_test() ->
    RufusText = "
    package example
    func Greeting() string { \"Hello\" }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"Hello">>}, example:'Greeting'()).

%% Arity-1 functions taking an unused argument

eval_with_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText = "
    package example
    func MaybeEcho(n bool) bool { true }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({bool, true}, example:'MaybeEcho'({bool, false})).

eval_with_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText = "
    package example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({float, 3.14159265359}, example:'MaybeEcho'({float, 3.14})).

eval_with_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText = "
    package example
    func MaybeEcho(n int) int { 42 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({int, 42}, example:'MaybeEcho'({int, 42})).

eval_with_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText = "
    package example
    func MaybeEcho(n string) string { \"Hello\" }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"Hello">>}, example:'MaybeEcho'({string, <<"Good morning">>})).

%% Arity-1 functions taking and using an argument

eval_with_function_taking_a_bool_and_returning_a_bool_test() ->
    RufusText = "
    package example
    func Echo(n bool) bool { n }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({bool, true}, example:'Echo'({bool, false})).

%% Type checking return values

eval_with_function_having_unmatched_return_types_test() ->
    RufusText = "
    package example
    func Number() float { 42 }
    ",
    {error, unmatched_return_type, _Data} = rufus_compile:eval(RufusText).
