-module(rfc_leex_test).

-include_lib("eunit/include/eunit.hrl").

% Packages

empty_package_test() ->
    {ok, Tokens, _} = rfc_leex:string("package empty"),
    [{package, 1, "package"}, {identifier, 1, "empty"}] = Tokens.

import_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package foo
import \"bar\""),
    [
     {package, 2, "package"}, {identifier, 2, "foo"},
     {import, 3, "import"}, {string, 3, "bar"}
    ] = Tokens.

% Constants

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

% Functions

arity0_function_returns_a_float_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package arity0
func number42() float { 42.0 }
"),
    [
     {package,  2, "package"}, {identifier, 2, "arity0"},
     {func, 3, "func"}, {identifier, 3, "number42"},
     {paren_begin, 3, "("}, {paren_end, 3, ")"}, {float_type, 3, "float"},
     {block_begin, 3, "{"}, {float, 3, "42.0"}, {block_end, 3, "}"}
    ] = Tokens.

arity0_function_returns_an_int_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package arity0
func number42() int { 42 }
"),
    [
     {package,  2, "package"}, {identifier, 2, "arity0"},
     {func, 3, "func"}, {identifier, 3, "number42"},
     {paren_begin, 3, "("}, {paren_end, 3, ")"}, {int_type, 3, "int"},
     {block_begin, 3, "{"}, {int, 3, "42"}, {block_end, 3, "}"}
    ] = Tokens.

arity0_function_returns_a_string_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package arity0
func int42() string { \"hello\" }
"),
    [
     {package,  2, "package"}, {identifier, 2, "arity0"},
     {func, 3, "func"}, {identifier, 3, "int42"},
     {paren_begin, 3, "("}, {paren_end, 3, ")"}, {string_type, 3, "string"},
     {block_begin, 3, "{"}, {string, 3, "hello"}, {block_end, 3, "}"}
    ] = Tokens.

arity0_multiline_function_returns_a_string_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package arity0
func int42() string {
    \"hello\"
}
"),
    [
     {package,  2, "package"}, {identifier, 2, "arity0"},
     {func, 3, "func"}, {identifier, 3, "int42"},
     {paren_begin, 3, "("}, {paren_end, 3, ")"}, {string_type, 3, "string"},
     {block_begin, 3, "{"}, {string, 4, "hello"}, {block_end, 5, "}"}
    ] = Tokens.

arity1_function_takes_an_int_and_returns_an_int_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package arity1
func echo(n int) int { n } "),
    [
     {package,  2, "package"}, {identifier, 2, "arity1"},
     {func, 3, "func"}, {identifier, 3, "echo"},
     {paren_begin, 3, "("}, {identifier, 3, "n"}, {int_type, 3, "int"}, {paren_end, 3, ")"}, {int_type, 3, "int"},
     {block_begin, 3, "{"}, {identifier, 3, "n"}, {block_end, 3, "}"}
    ] = Tokens.

arity2_function_tokes_an_int_and_a_string_and_returns_a_float_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package arity2
func float42(n int, s string) float { 42.0 }"),
    [
     {package,  2, "package"}, {identifier, 2, "arity2"},
     {func, 3, "func"}, {identifier, 3, "float42"},
     {paren_begin, 3, "("},
     {identifier, 3, "n"}, {int_type, 3, "int"}, {comma, 3, ","},
     {identifier, 3, "s"}, {string_type, 3, "string"},
     {paren_end, 3, ")"}, {float_type, 3, "float"},
     {block_begin, 3, "{"}, {float, 3, "42.0"}, {block_end, 3, "}"}
    ] = Tokens.
