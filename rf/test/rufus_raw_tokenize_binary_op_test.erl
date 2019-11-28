-module(rufus_raw_tokenize_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

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