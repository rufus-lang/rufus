-module(rufus_benchmark_test).

-include_lib("eunit/include/eunit.hrl").

untyped_some_arguments_function_calls_test() ->
    Iterations = 10000000,
    StartTime = os:timestamp(),
    benchmark(Iterations, fun() ->
        some_arguments(1, 1, 1, 1)
    end),
    Duration = time_diff(StartTime),
    io:format("time spent  => ~p~n", [Duration]),
    ?assert(false).

typed_some_arguments_function_calls_test() ->
    Iterations = 10000000,
    StartTime = os:timestamp(),
    benchmark(Iterations, fun() ->
        some_typed_arguments({int, int, int, int}, 1, 1, 1, 1)
    end),
    Duration = time_diff(StartTime),
    io:format("time spent  => ~p~n", [Duration]),
    ?assert(false).

untyped_many_arguments_function_calls_test() ->
    Iterations = 10000000,
    StartTime = os:timestamp(),
    benchmark(Iterations, fun() ->
        many_arguments(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    end),
    Duration = time_diff(StartTime),
    io:format("time spent  => ~p~n", [Duration]),
    ?assert(false).

typed_many_arguments_function_calls_test() ->
    Iterations = 10000000,
    StartTime = os:timestamp(),
    benchmark(Iterations, fun() ->
        many_typed_arguments({int, int, int, int, int, int, int, int, int, int, int, int, int}, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    end),
    Duration = time_diff(StartTime),
    io:format("time spent  => ~p~n", [Duration]),
    ?assert(false).

interleaved_typed_many_arguments_function_calls_test() ->
    Iterations = 10000000,
    StartTime = os:timestamp(),
    benchmark(Iterations, fun() ->
        interleaved_many_typed_arguments(int, 1, int, 1, int, 1, int, 1, int, 1, int, 1, int, 1, int, 1, int, 1, int, 1, int, 1, int, 1, int, 1)
    end),
    Duration = time_diff(StartTime),
    io:format("time spent  => ~p~n", [Duration]),
    ?assert(false).

untyped_function_calls_test() ->
    Iterations = 10000000,
    StartTime = os:timestamp(),
    benchmark(Iterations, fun() ->
        untyped_sum(1, 1)
    end),
    Duration = time_diff(StartTime),
    io:format("time spent  => ~p~n", [Duration]),
    ?assert(false).

typed_function_calls_test() ->
    Iterations = 10000000,
    StartTime = os:timestamp(),
    benchmark(Iterations, fun() ->
        typed_sum({int, int}, 1, 1)
    end),
    Duration = time_diff(StartTime),
    io:format("time spent  => ~p~n", [Duration]),
    ?assert(false).

interleaved_typed_function_calls_test() ->
    Iterations = 10000000,
    StartTime = os:timestamp(),
    benchmark(Iterations, fun() ->
        interleaved_typed_sum(int, 1, int, 1)
    end),
    Duration = time_diff(StartTime),
    io:format("time spent  => ~p~n", [Duration]),
    ?assert(false).

%% Test helper functions

time_diff(Init) ->
    timer:now_diff(os:timestamp(), Init) / 1000000.

benchmark(0, _Fn) ->
    ok;
benchmark(Repetitions, Fn) ->
    Fn(),
    benchmark(Repetitions-1, Fn).

untyped_sum(A, B) ->
    A + B.

typed_sum({int, int}, A, B) ->
    A + B.

interleaved_typed_sum(int, A, int, B) ->
    A + B.

some_arguments(_, _, _, _) ->
    ok.

some_typed_arguments({int, int, int, int}, _, _, _, _) ->
    ok.

many_arguments(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
    ok.

many_typed_arguments({int, int, int, int, int, int, int, int, int, int, int, int, int}, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    ok.

interleaved_many_typed_arguments(int, _, int, _, int, _, int, _, int, _, int, _, int, _, int, _, int, _, int, _, int, _, int, _, int, _) ->
    ok.
