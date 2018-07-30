-module(rfc_leex_const_test).

-include_lib("eunit/include/eunit.hrl").

float_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const Float = 3.1415"),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "Float"}, {match, 3, "="}, {float, 3, "3.1415"}
    ] = Tokens.

negative_float_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const NegativeFloat = -3.1415"),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "NegativeFloat"}, {match, 3, "="}, {float, 3, "-3.1415"}
    ] = Tokens.

float_with_positive_exponent_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const FloatWithPositiveExponent = 4.0e+2"),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "FloatWithPositiveExponent"}, {match, 3, "="}, {float, 3, "4.0e+2"}
    ] = Tokens.

negative_float_with_positive_exponent_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const NegativeFloatWithPositiveExponent = -4.0e+2"),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "NegativeFloatWithPositiveExponent"}, {match, 3, "="}, {float, 3, "-4.0e+2"}
    ] = Tokens.

float_with_negative_exponent_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const FloatWithNegativeExponent = 48.0e-2"),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "FloatWithNegativeExponent"}, {match, 3, "="}, {float, 3, "48.0e-2"}
    ] = Tokens.

negative_float_with_negative_exponent_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const NegativeFloatWithNegativeExponent = -48.0e-2"),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "NegativeFloatWithNegativeExponent"}, {match, 3, "="}, {float, 3, "-48.0e-2"}
    ] = Tokens.

int_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const Int = 1"),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "Int"}, {match, 3, "="}, {int, 3, "1"}
    ] = Tokens.

negative_int_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const NegativeInt = -1"),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "NegativeInt"}, {match, 3, "="}, {int, 3, "-1"}
    ] = Tokens.

string_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const Name = \"Rufus\""),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "Name"}, {match, 3, "="}, {string, 3, "Rufus"}
    ] = Tokens.

string_with_whitespace_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package literal
const Whitespace = \"hello world\""),
    [
     {package, 2, "package"}, {identifier, 2, "literal"},
     {const_type, 3, "const"}, {identifier, 3, "Whitespace"}, {match, 3, "="}, {string, 3, "hello world"}
    ] = Tokens.
