-module(rfc_leex_const_test).

-include_lib("eunit/include/eunit.hrl").

float_test() ->
    {ok, Tokens, _} = rfc_leex:string("const Float = 3.1415"),
    [
     {const, 1, "const"},
     {identifier, 1, "Float"},
     {match, 1, "="},
     {float_lit, 1, "3.1415"}
    ] = Tokens.

negative_float_test() ->
    {ok, Tokens, _} = rfc_leex:string("const NegativeFloat = -3.1415"),
    [
     {const, 1, "const"},
     {identifier, 1, "NegativeFloat"},
     {match, 1, "="},
     {float_lit, 1, "-3.1415"}
    ] = Tokens.

float_with_positive_exponent_test() ->
    {ok, Tokens, _} = rfc_leex:string("const FloatWithPositiveExponent = 4.0e+2"),
    [
     {const, 1, "const"},
     {identifier, 1, "FloatWithPositiveExponent"},
     {match, 1, "="},
     {float_lit, 1, "4.0e+2"}
    ] = Tokens.

negative_float_with_positive_exponent_test() ->
    {ok, Tokens, _} = rfc_leex:string("const NegativeFloatWithPositiveExponent = -4.0e+2"),
    [
     {const, 1, "const"},
     {identifier, 1, "NegativeFloatWithPositiveExponent"},
     {match, 1, "="},
     {float_lit, 1, "-4.0e+2"}
    ] = Tokens.

float_with_negative_exponent_test() ->
    {ok, Tokens, _} = rfc_leex:string("const FloatWithNegativeExponent = 48.0e-2"),
    [
     {const, 1, "const"},
     {identifier, 1, "FloatWithNegativeExponent"},
     {match, 1, "="},
     {float_lit, 1, "48.0e-2"}
    ] = Tokens.

negative_float_with_negative_exponent_test() ->
    {ok, Tokens, _} = rfc_leex:string("const NegativeFloatWithNegativeExponent = -48.0e-2"),
    [
     {const, 1, "const"},
     {identifier, 1, "NegativeFloatWithNegativeExponent"},
     {match, 1, "="},
     {float_lit, 1, "-48.0e-2"}
    ] = Tokens.

int_test() ->
    {ok, Tokens, _} = rfc_leex:string("const Int = 1"),
    [
     {const, 1, "const"},
     {identifier, 1, "Int"},
     {match, 1, "="},
     {int_lit, 1, "1"}
    ] = Tokens.

negative_int_test() ->
    {ok, Tokens, _} = rfc_leex:string("const NegativeInt = -1"),
    [
     {const, 1, "const"},
     {identifier, 1, "NegativeInt"},
     {match, 1, "="},
     {int_lit, 1, "-1"}
    ] = Tokens.

string_test() ->
    {ok, Tokens, _} = rfc_leex:string("const Name = \"Rufus\""),
    [
     {const, 1, "const"},
     {identifier, 1, "Name"},
     {match, 1, "="},
     {string_lit, 1, "Rufus"}
    ] = Tokens.

string_with_whitespace_test() ->
    {ok, Tokens, _} = rfc_leex:string("const Whitespace = \"hello world\""),
    [
     {const, 1, "const"},
     {identifier, 1, "Whitespace"},
     {match, 1, "="},
     {string_lit, 1, "hello world"}
    ] = Tokens.

% string_with_punctuation_test
% string_with_multibyte_utf8_character_test
