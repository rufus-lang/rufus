-module(rufus_raw_tokenize_case_test).

-include_lib("eunit/include/eunit.hrl").

string_with_case_block_with_single_clause_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "case 42 {\n"
        "match 42 ->\n"
        "    37\n"
        "}\n"
    ),
    ?assertEqual(
        [
            {'case', 1},
            {int_lit, 1, 42},
            {'{', 1},
            {eol, 1},
            {match, 2},
            {int_lit, 2, 42},
            {'->', 2},
            {eol, 2},
            {int_lit, 3, 37},
            {eol, 3},
            {'}', 4},
            {eol, 4}
        ],
        Tokens
    ).

string_with_case_block_with_multiple_clauses_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "case 42 {\n"
        "match 42 ->\n"
        "    37\n"
        "match 13 ->\n"
        "    19\n"
        "}\n"
    ),
    ?assertEqual(
        [
            {'case', 1},
            {int_lit, 1, 42},
            {'{', 1},
            {eol, 1},
            {match, 2},
            {int_lit, 2, 42},
            {'->', 2},
            {eol, 2},
            {int_lit, 3, 37},
            {eol, 3},
            {match, 4},
            {int_lit, 4, 13},
            {'->', 4},
            {eol, 4},
            {int_lit, 5, 19},
            {eol, 5},
            {'}', 6},
            {eol, 6}
        ],
        Tokens
    ).
