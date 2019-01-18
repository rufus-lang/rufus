-module(rufus_parse_test).

-include_lib("eunit/include/eunit.hrl").

%% Packages

parse_empty_package_test() ->
    {ok, Tokens, _} = rufus_scan:string("package empty"),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 1, "empty"}
    ], Forms).

%% Import

parse_import_test() ->
    RufusText = "
    package foo
    import \"bar\"
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "foo"},
     {import, 3, "bar"}
    ], Forms).

%% Arity-0 functions returning a literal value for primitive types

parse_function_returning_a_bool_test() ->
    RufusText = "
    package example
    func True() bool { true }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "example"},
     {func, 3, "True", [], bool, [{expr, 3, {bool, true}}]}
    ], Forms).

parse_function_returning_a_float_test() ->
    RufusText = "
    package math
    func Pi() float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "math"},
     {func, 3, "Pi", [], float, [{expr, 3, {float, 3.14159265359}}]}
    ], Forms).

parse_function_returning_an_int_test() ->
    RufusText = "
    package rand
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "rand"},
     {func, 3, "Number", [], int, [{expr, 3, {int, 42}}]}
    ], Forms).

parse_function_returning_a_string_test() ->
    RufusText = "
    package example
    func Greeting() string { \"Hello\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "example"},
     {func, 3, "Greeting", [], string, [{expr, 3, {string, "Hello"}}]}
    ], Forms).

%% Arity-1 functions using an argument

parse_function_taking_an_bool_and_returning_an_bool_test() ->
    RufusText = "
    package example
    func Echo(n bool) bool { true }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "example"},
     {func, 3, "Echo", [{arg, 3, n, bool}], bool, [{expr, 3, {bool, true}}]}
    ], Forms).

parse_function_taking_an_float_and_returning_an_float_test() ->
    RufusText = "
    package example
    func Echo(n float) float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "example"},
     {func, 3, "Echo", [{arg, 3, n, float}], float, [{expr, 3, {float, 3.14159265359}}]}
    ], Forms).

parse_function_taking_an_int_and_returning_an_int_test() ->
    RufusText = "
    package example
    func Echo(n int) int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "example"},
     {func, 3, "Echo", [{arg, 3, n, int}], int, [{expr, 3, {int, 42}}]}
    ], Forms).

parse_function_taking_an_string_and_returning_an_string_test() ->
    RufusText = "
    package example
    func Echo(n string) string { \"Hello\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {package, 2, "example"},
     {func, 3, "Echo", [{arg, 3, n, string}], string, [{expr, 3, {string, "Hello"}}]}
    ], Forms).
