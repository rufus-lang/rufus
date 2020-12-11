-module(rufus_raw_tokenize_try_catch_test).

-include_lib("eunit/include/eunit.hrl").

string_with_function_returning_a_bool_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("catch { match :error -> :error }"),
    ?assertEqual(
        [
            {identifier, 1, "catch"},
            {'{', 1},
            {match, 1},
            {atom_lit, 1, error},
            {'->', 1},
            {atom_lit, 1, error},
            {'}', 1}
        ],
        Tokens
    ).
