-module(rufus_compile_func_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a literal value for scalar types

eval_with_function_returning_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping() atom { :pong }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(pong, example:'Ping'()).

eval_with_function_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func True() bool { true }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({bool, true}, example:'True'()).

eval_with_function_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func Pi() float { 3.14159265359 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3.14159265359, example:'Pi'()).

eval_with_function_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(42, example:'Number'()).

eval_with_function_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func Greeting() string { \"Hello\" }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"Hello">>}, example:'Greeting'()).

%% Arity-0 functions with multiple function expressions

forms_for_function_with_multiple_expressions_test() ->
    RufusText = "
    module example
    func Multiple() atom {
        42
        :fortytwo
    }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(fortytwo, example:'Multiple'()).

%% Arity-1 functions taking an unused parameter for scalar types

eval_with_function_taking_an_atom_and_returning_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping(m atom) atom { :pong }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(pong, example:'Ping'(hello)).

eval_with_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n bool) bool { true }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({bool, true}, example:'MaybeEcho'({bool, false})).

eval_with_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'MaybeEcho'(3.14)).

eval_with_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n int) int { 42 }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'MaybeEcho'(42)).

eval_with_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n string) string { \"Hello\" }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"Hello">>}, example:'MaybeEcho'({string, <<"Good morning">>})).

%% Arity-1 functions taking an argument and returning it for scalar types

eval_with_function_taking_an_atom_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(m atom) atom { m }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(hello, example:'Echo'(hello)).

eval_with_function_taking_a_bool_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(b bool) bool { b }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({bool, false}, example:'Echo'({bool, false})).

eval_with_function_taking_a_float_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(n float) float { n }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(3.14159265359, example:'Echo'(3.14159265359)).

eval_with_function_taking_an_int_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual(42, example:'Echo'(42)).

eval_with_function_taking_a_string_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(s string) string { s }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual({string, <<"Hello">>}, example:'Echo'({string, <<"Hello">>})).

%% Type checking return values

eval_with_function_having_unmatched_return_types_test() ->
    RufusText = "
    module example
    func Number() float { 42 }
    ",
    Data = #{expr => {int_lit, #{line => 3,
                                 spec => 42,
                                 type => {type, #{line => 3,
                                                  source => inferred,
                                                  spec => int}}}},
             return_type => {type, #{line => 3,
                                     source => rufus_text,
                                     spec => float}}},
    ?assertEqual({error, unmatched_return_type, Data}, rufus_compile:eval(RufusText)).

%% Arity-1 functions taking and returning an argument with a mismatched return
%% type

eval_with_function_taking_a_bool_and_returning_it_with_a_mismatched_return_type_test() ->
    RufusText = "
    module example
    func MismatchedReturnType(b bool) int { b }
    ",
    Data = #{expr => {identifier, #{line => 3,
                                    spec => b,
                                    type => {type, #{line => 3,
                                                     source => rufus_text,
                                                     spec => bool}}}},
             return_type => {type, #{line => 3,
                                     source => rufus_text,
                                     spec => int}}},
    ?assertEqual({error, unmatched_return_type, Data}, rufus_compile:eval(RufusText)).