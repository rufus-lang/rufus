-module(rufus_raw_tokenize_test).

-include_lib("eunit/include/eunit.hrl").

%% Modules

string_with_empty_module_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("module empty"),
    ?assertEqual([
     {module, 1},
     {identifier, 1, "empty"}
    ], Tokens).

string_with_import_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("
     module foo
     import \"bar\"
    "),
    ?assertEqual([
     {eol, 1},
     {module, 2},
     {identifier, 2, "foo"},
     {eol, 2},
     {import, 3},
     {string_lit, 3, "bar"},
     {eol, 3}
], Tokens).

%% Const

string_with_atom_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Name = :rufus"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Name"},
     {'=', 1},
     {atom_lit, 1, rufus}
    ], Tokens).

string_with_quoted_atom_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Name = :'rufus programming language'"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Name"},
     {'=', 1},
     {atom_lit, 1, 'rufus programming language'}
    ], Tokens).

string_with_false_bool_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Bool = false"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Bool"},
     {'=', 1},
     {bool_lit, 1, false}
    ], Tokens).

string_with_true_bool_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Bool = true"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Bool"},
     {'=', 1},
     {bool_lit, 1, true}
    ], Tokens).

string_with_float_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Float = 3.1415"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Float"},
     {'=', 1},
     {float_lit, 1, 3.1415}
    ], Tokens).

string_with_negative_float_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const NegativeFloat = -3.1415"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "NegativeFloat"},
     {'=', 1},
     {float_lit, 1, -3.1415}
    ], Tokens).

string_with_positive_float_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const PositiveFloat = +3.1415"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "PositiveFloat"},
     {'=', 1},
     {float_lit, 1, 3.1415}
    ], Tokens).

string_with_float_with_positive_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const FloatWithPositiveExponent = 4.0e+2"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "FloatWithPositiveExponent"},
     {'=', 1},
     {float_lit, 1, 4.0e+2}
    ], Tokens).

string_with_negative_float_with_positive_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const NegativeFloatWithPositiveExponent = -4.0e+2"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "NegativeFloatWithPositiveExponent"},
     {'=', 1},
     {float_lit, 1, -4.0e+2}
    ], Tokens).

string_with_positive_float_with_positive_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const PositiveFloatWithPositiveExponent = +4.0e+2"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "PositiveFloatWithPositiveExponent"},
     {'=', 1},
     {float_lit, 1, 4.0e+2}
    ], Tokens).

string_with_float_with_negative_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const FloatWithNegativeExponent = 48.0e-2"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "FloatWithNegativeExponent"},
     {'=', 1},
     {float_lit, 1, 48.0e-2}
    ], Tokens).

string_with_negative_float_with_negative_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const NegativeFloatWithNegativeExponent = -48.0e-2"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "NegativeFloatWithNegativeExponent"},
     {'=', 1},
     {float_lit, 1, -48.0e-2}
    ], Tokens).

string_with_positive_float_with_negative_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const PositiveFloatWithNegativeExponent = +48.0e-2"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "PositiveFloatWithNegativeExponent"},
     {'=', 1},
     {float_lit, 1, 48.0e-2}
    ], Tokens).

string_with_int_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Int = 1"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Int"},
     {'=', 1},
     {int_lit, 1, 1}
    ], Tokens).

string_with_negative_int_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const NegativeInt = -1"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "NegativeInt"},
     {'=', 1},
     {int_lit, 1, -1}
    ], Tokens).

string_with_positive_int_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const PositiveInt = +1"),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "PositiveInt"},
     {'=', 1},
     {int_lit, 1, 1}
    ], Tokens).

string_with_string_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Name = \"Rufus\""),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Name"},
     {'=', 1},
     {string_lit, 1, "Rufus"}
    ], Tokens).

string_with_string_containing_number_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Number = \"42\""),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Number"},
     {'=', 1},
     {string_lit, 1, "42"}
    ], Tokens).

string_with_string_containing_whitespace_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Whitespace = \"hello world\""),
    ?assertEqual([
     {const, 1},
     {identifier, 1, "Whitespace"},
     {'=', 1},
     {string_lit, 1, "hello world"}
    ], Tokens).

% string_with_string_containing_punctuation_test
% string_with_string_containing_multibyte_utf8_character_test

%% Functions

string_with_function_returning_a_bool_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func False() bool { false }"),
    ?assertEqual([
     {func, 1},
     {identifier, 1, "False"},
     {'(', 1},
     {')', 1},
     {bool, 1},
     {'{', 1},
     {bool_lit, 1, false},
     {'}', 1}
    ], Tokens).

string_with_function_returning_a_float_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func number() float { 42.0 }"),
    ?assertEqual([
     {func, 1},
     {identifier, 1, "number"},
     {'(', 1},
     {')', 1},
     {float, 1},
     {'{', 1},
     {float_lit, 1, 42.0},
     {'}', 1}
    ], Tokens).

string_with_function_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func number() int { 42 }"),
    ?assertEqual([
     {func, 1},
     {identifier, 1, "number"},
     {'(', 1},
     {')', 1},
     {int, 1},
     {'{', 1},
     {int_lit, 1, 42},
     {'}', 1}
    ], Tokens).

string_with_function_returning_a_string_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func text() string { \"42\" }"),
    ?assertEqual([
     {func, 1},
     {identifier, 1, "text"},
     {'(', 1},
     {')', 1},
     {string, 1},
     {'{', 1},
     {string_lit, 1, "42"},
     {'}', 1}
    ], Tokens).

string_with_multiline_function_returning_a_string_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("
func text() string {
    \"42\"
}
"),
    ?assertEqual([
     {eol, 1},
     {func, 2},
     {identifier, 2, "text"},
     {'(', 2},
     {')', 2},
     {string, 2},
     {'{', 2},
     {eol, 2},
     {string_lit, 3, "42"},
     {eol, 3},
     {'}', 4},
     {eol, 4}
    ], Tokens).

string_with_multiline_multiple_expression_function_returning_an_atom_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("
func number() atom {
    \"42\"
    :fortytwo
}
"),
    ?assertEqual([
     {eol, 1},
     {func, 2},
     {identifier, 2, "number"},
     {'(', 2},
     {')', 2},
     {atom, 2},
     {'{', 2},
     {eol, 2},
     {string_lit, 3, "42"},
     {eol, 3},
     {atom_lit, 4, fortytwo},
     {eol, 4},
     {'}', 5},
     {eol, 5}
    ], Tokens).

string_with_semicolon_multiple_expression_function_returning_an_atom_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func number() atom { \"42\"; :fortytwo }"),
    ?assertEqual([
     {func, 1},
     {identifier, 1, "number"},
     {'(', 1},
     {')', 1},
     {atom, 1},
     {'{', 1},
     {string_lit, 1, "42"},
     {';', 1},
     {atom_lit, 1, fortytwo},
     {'}', 1}
    ], Tokens).

string_with_function_takes_an_int_and_returning_an_int_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func echo(n int) int { n }"),
    ?assertEqual([
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
    ], Tokens).

string_with_function_tokes_an_int_and_a_string_and_returning_a_float_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func number(n int, s string) float { 42.0 }"),
    ?assertEqual([
     {func, 1},
     {identifier, 1, "number"},
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
    ], Tokens).

string_with_binary_op_expression_using_plus_operator_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("3 + 5"),
    ?assertEqual([
     {int_lit, 1, 3},
     {'+', 1},
     {int_lit, 1, 5}
    ], Tokens).

string_with_binary_op_expression_using_minus_operator_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("3 - 5"),
    ?assertEqual([
     {int_lit, 1, 3},
     {'-', 1},
     {int_lit, 1, 5}
    ], Tokens).

string_with_binary_op_expression_using_multiplication_operator_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("3 * 5"),
    ?assertEqual([
     {int_lit, 1, 3},
     {'*', 1},
     {int_lit, 1, 5}
    ], Tokens).

string_with_binary_op_expression_using_division_operator_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("3 / 5"),
    ?assertEqual([
     {int_lit, 1, 3},
     {'/', 1},
     {int_lit, 1, 5}
    ], Tokens).

string_with_binary_op_expression_using_remainder_operator_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("3 % 5"),
    ?assertEqual([
     {int_lit, 1, 3},
     {'%', 1},
     {int_lit, 1, 5}
    ], Tokens).

%% string_with_function_takes_an_unused_argument_test() ->
%%     {ok, Tokens, _} = rufus_raw_tokenize:string("func unused(_ int) int { 0 }")

%% string_with_function_takes_an_unused_named_argument_test() ->
%%     {ok, Tokens, _} = rufus_raw_tokenize:string("func unused(_num int) int { 0 }")
