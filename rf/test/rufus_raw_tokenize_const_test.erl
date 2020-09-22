-module(rufus_raw_tokenize_const_test).

-include_lib("eunit/include/eunit.hrl").

string_with_atom_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Name = :rufus"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Name"},
            {'=', 1},
            {atom_lit, 1, rufus}
        ],
        Tokens
    ).

string_with_single_character_atom_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Letter = :r"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Letter"},
            {'=', 1},
            {atom_lit, 1, r}
        ],
        Tokens
    ).

string_with_quoted_atom_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Name = :'rufus programming language'"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Name"},
            {'=', 1},
            {atom_lit, 1, 'rufus programming language'}
        ],
        Tokens
    ).

string_with_quoted_single_character_atom_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Letter = :'r'"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Letter"},
            {'=', 1},
            {atom_lit, 1, r}
        ],
        Tokens
    ).

string_with_false_bool_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Bool = false"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Bool"},
            {'=', 1},
            {bool_lit, 1, false}
        ],
        Tokens
    ).

string_with_true_bool_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Bool = true"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Bool"},
            {'=', 1},
            {bool_lit, 1, true}
        ],
        Tokens
    ).

string_with_float_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Float = 3.1415"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Float"},
            {'=', 1},
            {float_lit, 1, 3.1415}
        ],
        Tokens
    ).

string_with_negative_float_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const NegativeFloat = -3.1415"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "NegativeFloat"},
            {'=', 1},
            {float_lit, 1, -3.1415}
        ],
        Tokens
    ).

string_with_positive_float_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const PositiveFloat = +3.1415"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "PositiveFloat"},
            {'=', 1},
            {float_lit, 1, 3.1415}
        ],
        Tokens
    ).

string_with_float_with_positive_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const FloatWithPositiveExponent = 4.0e+2"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "FloatWithPositiveExponent"},
            {'=', 1},
            {float_lit, 1, 4.0e+2}
        ],
        Tokens
    ).

string_with_negative_float_with_positive_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "const NegativeFloatWithPositiveExponent = -4.0e+2"
    ),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "NegativeFloatWithPositiveExponent"},
            {'=', 1},
            {float_lit, 1, -4.0e+2}
        ],
        Tokens
    ).

string_with_positive_float_with_positive_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "const PositiveFloatWithPositiveExponent = +4.0e+2"
    ),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "PositiveFloatWithPositiveExponent"},
            {'=', 1},
            {float_lit, 1, 4.0e+2}
        ],
        Tokens
    ).

string_with_float_with_negative_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const FloatWithNegativeExponent = 48.0e-2"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "FloatWithNegativeExponent"},
            {'=', 1},
            {float_lit, 1, 48.0e-2}
        ],
        Tokens
    ).

string_with_negative_float_with_negative_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "const NegativeFloatWithNegativeExponent = -48.0e-2"
    ),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "NegativeFloatWithNegativeExponent"},
            {'=', 1},
            {float_lit, 1, -48.0e-2}
        ],
        Tokens
    ).

string_with_positive_float_with_negative_exponent_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "const PositiveFloatWithNegativeExponent = +48.0e-2"
    ),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "PositiveFloatWithNegativeExponent"},
            {'=', 1},
            {float_lit, 1, 48.0e-2}
        ],
        Tokens
    ).

string_with_int_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Int = 1"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Int"},
            {'=', 1},
            {int_lit, 1, 1}
        ],
        Tokens
    ).

string_with_negative_int_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const NegativeInt = -1"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "NegativeInt"},
            {'=', 1},
            {int_lit, 1, -1}
        ],
        Tokens
    ).

string_with_positive_int_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const PositiveInt = +1"),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "PositiveInt"},
            {'=', 1},
            {int_lit, 1, 1}
        ],
        Tokens
    ).

string_with_string_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Name = \"Rufus\""),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Name"},
            {'=', 1},
            {string_lit, 1, "Rufus"}
        ],
        Tokens
    ).

string_with_string_containing_number_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Number = \"42\""),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Number"},
            {'=', 1},
            {string_lit, 1, "42"}
        ],
        Tokens
    ).

string_with_string_containing_whitespace_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("const Whitespace = \"hello world\""),
    ?assertEqual(
        [
            {const, 1},
            {identifier, 1, "Whitespace"},
            {'=', 1},
            {string_lit, 1, "hello world"}
        ],
        Tokens
    ).

%% TODO(jkakar): Implement these tests:
%% - string_with_string_containing_punctuation_test
%% - string_with_string_containing_multibyte_utf8_character_test
