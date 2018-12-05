-module(rfc_scan_const_test).

-include_lib("eunit/include/eunit.hrl").

float_test() ->
    {ok, Tokens, _} = rfc_scan:string("const Float = 3.1415"),
    [
     {const, 1},
     {identifier, 1, "Float"},
     {'=', 1},
     {float_lit, 1, 3.1415}
    ] = Tokens.

negative_float_test() ->
    {ok, Tokens, _} = rfc_scan:string("const NegativeFloat = -3.1415"),
    [
     {const, 1},
     {identifier, 1, "NegativeFloat"},
     {'=', 1},
     {float_lit, 1, -3.1415}
    ] = Tokens.

float_with_positive_exponent_test() ->
    {ok, Tokens, _} = rfc_scan:string("const FloatWithPositiveExponent = 4.0e+2"),
    [
     {const, 1},
     {identifier, 1, "FloatWithPositiveExponent"},
     {'=', 1},
     {float_lit, 1, 4.0e+2}
    ] = Tokens.

negative_float_with_positive_exponent_test() ->
    {ok, Tokens, _} = rfc_scan:string("const NegativeFloatWithPositiveExponent = -4.0e+2"),
    [
     {const, 1},
     {identifier, 1, "NegativeFloatWithPositiveExponent"},
     {'=', 1},
     {float_lit, 1, -4.0e+2}
    ] = Tokens.

float_with_negative_exponent_test() ->
    {ok, Tokens, _} = rfc_scan:string("const FloatWithNegativeExponent = 48.0e-2"),
    [
     {const, 1},
     {identifier, 1, "FloatWithNegativeExponent"},
     {'=', 1},
     {float_lit, 1, 48.0e-2}
    ] = Tokens.

negative_float_with_negative_exponent_test() ->
    {ok, Tokens, _} = rfc_scan:string("const NegativeFloatWithNegativeExponent = -48.0e-2"),
    [
     {const, 1},
     {identifier, 1, "NegativeFloatWithNegativeExponent"},
     {'=', 1},
     {float_lit, 1, -48.0e-2}
    ] = Tokens.

int_test() ->
    {ok, Tokens, _} = rfc_scan:string("const Int = 1"),
    [
     {const, 1},
     {identifier, 1, "Int"},
     {'=', 1},
     {int_lit, 1, 1}
    ] = Tokens.

negative_int_test() ->
    {ok, Tokens, _} = rfc_scan:string("const NegativeInt = -1"),
    [
     {const, 1},
     {identifier, 1, "NegativeInt"},
     {'=', 1},
     {int_lit, 1, -1}
    ] = Tokens.

string_test() ->
    {ok, Tokens, _} = rfc_scan:string("const Name = \"Rufus\""),
    [
     {const, 1},
     {identifier, 1, "Name"},
     {'=', 1},
     {string_lit, 1, "Rufus"}
    ] = Tokens.

string_with_numnber_test() ->
    {ok, Tokens, _} = rfc_scan:string("const Number = \"42\""),
    [
     {const, 1},
     {identifier, 1, "Number"},
     {'=', 1},
     {string_lit, 1, "42"}
    ] = Tokens.

string_with_whitespace_test() ->
    {ok, Tokens, _} = rfc_scan:string("const Whitespace = \"hello world\""),
    [
     {const, 1},
     {identifier, 1, "Whitespace"},
     {'=', 1},
     {string_lit, 1, "hello world"}
    ] = Tokens.

% string_with_punctuation_test
% string_with_multibyte_utf8_character_test
