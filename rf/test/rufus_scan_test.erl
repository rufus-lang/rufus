-module(rufus_scan_test).

-include_lib("eunit/include/eunit.hrl").

%% Packages

string_with_empty_package_test() ->
    {ok, Tokens, _} = rufus_scan:string("package empty"),
    [
     {package, 1},
     {identifier, 1, "empty"}
    ] = Tokens.

string_with_import_test() ->
    {ok, Tokens, _} = rufus_scan:string("
     package foo
     import \"bar\"
    "),
    [
     {package, 2},
     {identifier, 2, "foo"},
     {import, 3},
     {string_lit, 3, "bar"}
    ] = Tokens.

%% Const

string_with_float_test() ->
    {ok, Tokens, _} = rufus_scan:string("const Float = 3.1415"),
    [
     {const, 1},
     {identifier, 1, "Float"},
     {'=', 1},
     {float_lit, 1, 3.1415}
    ] = Tokens.

string_with_negative_float_test() ->
    {ok, Tokens, _} = rufus_scan:string("const NegativeFloat = -3.1415"),
    [
     {const, 1},
     {identifier, 1, "NegativeFloat"},
     {'=', 1},
     {float_lit, 1, -3.1415}
    ] = Tokens.

string_with_float_with_positive_exponent_test() ->
    {ok, Tokens, _} = rufus_scan:string("const FloatWithPositiveExponent = 4.0e+2"),
    [
     {const, 1},
     {identifier, 1, "FloatWithPositiveExponent"},
     {'=', 1},
     {float_lit, 1, 4.0e+2}
    ] = Tokens.

string_with_negative_float_with_positive_exponent_test() ->
    {ok, Tokens, _} = rufus_scan:string("const NegativeFloatWithPositiveExponent = -4.0e+2"),
    [
     {const, 1},
     {identifier, 1, "NegativeFloatWithPositiveExponent"},
     {'=', 1},
     {float_lit, 1, -4.0e+2}
    ] = Tokens.

string_with_float_with_negative_exponent_test() ->
    {ok, Tokens, _} = rufus_scan:string("const FloatWithNegativeExponent = 48.0e-2"),
    [
     {const, 1},
     {identifier, 1, "FloatWithNegativeExponent"},
     {'=', 1},
     {float_lit, 1, 48.0e-2}
    ] = Tokens.

string_with_negative_float_with_negative_exponent_test() ->
    {ok, Tokens, _} = rufus_scan:string("const NegativeFloatWithNegativeExponent = -48.0e-2"),
    [
     {const, 1},
     {identifier, 1, "NegativeFloatWithNegativeExponent"},
     {'=', 1},
     {float_lit, 1, -48.0e-2}
    ] = Tokens.

string_with_int_test() ->
    {ok, Tokens, _} = rufus_scan:string("const Int = 1"),
    [
     {const, 1},
     {identifier, 1, "Int"},
     {'=', 1},
     {int_lit, 1, 1}
    ] = Tokens.

string_with_negative_int_test() ->
    {ok, Tokens, _} = rufus_scan:string("const NegativeInt = -1"),
    [
     {const, 1},
     {identifier, 1, "NegativeInt"},
     {'=', 1},
     {int_lit, 1, -1}
    ] = Tokens.

string_with_string_test() ->
    {ok, Tokens, _} = rufus_scan:string("const Name = \"Rufus\""),
    [
     {const, 1},
     {identifier, 1, "Name"},
     {'=', 1},
     {string_lit, 1, "Rufus"}
    ] = Tokens.

string_with_string_containing_number_test() ->
    {ok, Tokens, _} = rufus_scan:string("const Number = \"42\""),
    [
     {const, 1},
     {identifier, 1, "Number"},
     {'=', 1},
     {string_lit, 1, "42"}
    ] = Tokens.

string_with_string_containing_whitespace_test() ->
    {ok, Tokens, _} = rufus_scan:string("const Whitespace = \"hello world\""),
    [
     {const, 1},
     {identifier, 1, "Whitespace"},
     {'=', 1},
     {string_lit, 1, "hello world"}
    ] = Tokens.

% string_with_string_containing_punctuation_test
% string_with_string_containing_multibyte_utf8_character_test

%% Functions

string_with_function_returning_a_float_test() ->
    {ok, Tokens, _} = rufus_scan:string("func number42() float { 42.0 }"),
    [
     {func, 1},
     {identifier, 1, "number42"},
     {'(', 1},
     {')', 1},
     {float, 1},
     {'{', 1},
     {float_lit, 1, 42.0},
     {'}', 1}
    ] = Tokens.

string_with_function_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_scan:string("func number42() int { 42 }"),
    [
     {func, 1},
     {identifier, 1, "number42"},
     {'(', 1},
     {')', 1},
     {int, 1},
     {'{', 1},
     {int_lit, 1, 42},
     {'}', 1}
    ] = Tokens.

string_with_function_returning_a_string_test() ->
    {ok, Tokens, _} = rufus_scan:string("func text42() string { \"42\" }"),
    [
     {func, 1},
     {identifier, 1, "text42"},
     {'(', 1},
     {')', 1},
     {string, 1},
     {'{', 1},
     {string_lit, 1, "42"},
     {'}', 1}
    ] = Tokens.

string_with_multiline_function_returning_a_string_test() ->
    {ok, Tokens, _} = rufus_scan:string("
func text42() string {
    \"42\"
}
"),
    [
     {func, 2},
     {identifier, 2, "text42"},
     {'(', 2},
     {')', 2},
     {string, 2},
     {'{', 2},
     {string_lit, 3, "42"},
     {'}', 4}
    ] = Tokens.

string_with_function_takes_an_int_and_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_scan:string("func echo(n int) int { n }"),
    [
     {func, 1},
     {identifier, 1, "echo"},
     {'(', 1},
     {identifier, 1, "n"},
     {int, 1},
     {')', 1},
     {int, 1},
     {'{', 1},
     {identifier, 1, "n"},
     {'}', 1}
    ] = Tokens.

string_with_function_tokes_an_int_and_a_string_and_returning_a_float_test() ->
    {ok, Tokens, _} = rufus_scan:string("func float42(n int, s string) float { 42.0 }"),
    [
     {func, 1},
     {identifier, 1, "float42"},
     {'(', 1},
     {identifier, 1, "n"},
     {int, 1},
     {',', 1},
     {identifier, 1, "s"},
     {string, 1},
     {')', 1},
     {float, 1},
     {'{', 1},
     {float_lit, 1, 42.0},
     {'}', 1}
    ] = Tokens.

%% string_with_function_takes_an_unused_argument_test() ->
%%     {ok, Tokens, _} = rufus_scan:string("func unused(_ int) int { 0 }")

%% string_with_function_takes_an_unused_named_argument_test() ->
%%     {ok, Tokens, _} = rufus_scan:string("func unused(_num int) int { 0 }")
