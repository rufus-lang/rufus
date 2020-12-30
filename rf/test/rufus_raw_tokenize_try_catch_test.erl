-module(rufus_raw_tokenize_try_catch_test).

-include_lib("eunit/include/eunit.hrl").

string_with_try_catch_block_with_single_clause_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "try {\n"
        "    \"42\"\n"
        "} catch :error {\n"
        "    :error\n"
        "}"
    ),
    ?assertEqual(
        [
            {'try', 1},
            {'{', 1},
            {eol, 1},
            {string_lit, 2, "42"},
            {eol, 2},
            {'}', 3},
            {'catch', 3},
            {atom_lit, 3, error},
            {'{', 3},
            {eol, 3},
            {atom_lit, 4, error},
            {eol, 4},
            {'}', 5}
        ],
        Tokens
    ).

string_with_try_catch_block_with_single_clause_and_after_block_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "try {\n"
        "    \"42\"\n"
        "} catch :error {\n"
        "    :error\n"
        "} after {\n"
        "    cleanup()\n"
        "}"
    ),
    ?assertEqual(
        [
            {'try', 1},
            {'{', 1},
            {eol, 1},
            {string_lit, 2, "42"},
            {eol, 2},
            {'}', 3},
            {'catch', 3},
            {atom_lit, 3, error},
            {'{', 3},
            {eol, 3},
            {atom_lit, 4, error},
            {eol, 4},
            {'}', 5},
            {'after', 5},
            {'{', 5},
            {eol, 5},
            {identifier, 6, "cleanup"},
            {'(', 6},
            {')', 6},
            {eol, 6},
            {'}', 7}
        ],
        Tokens
    ).

string_with_try_catch_block_with_multiple_clauses_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "try {\n"
        "    \"42\"\n"
        "} catch {\n"
        "match :first ->\n"
        "    :first\n"
        "match :second ->\n"
        "    :second\n"
        "}"
    ),
    ?assertEqual(
        [
            {'try', 1},
            {'{', 1},
            {eol, 1},
            {string_lit, 2, "42"},
            {eol, 2},
            {'}', 3},
            {'catch', 3},
            {'{', 3},
            {eol, 3},
            {match, 4},
            {atom_lit, 4, first},
            {'->', 4},
            {eol, 4},
            {atom_lit, 5, first},
            {eol, 5},
            {match, 6},
            {atom_lit, 6, second},
            {'->', 6},
            {eol, 6},
            {atom_lit, 7, second},
            {eol, 7},
            {'}', 8}
        ],
        Tokens
    ).

string_with_try_catch_block_with_multiple_clauses_and_after_block_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "try {\n"
        "    \"42\"\n"
        "} catch {\n"
        "match :first ->\n"
        "    :first\n"
        "match :second ->\n"
        "    :second\n"
        "} after {\n"
        "    cleanup()\n"
        "}\n"
    ),
    ?assertEqual(
        [
            {'try', 1},
            {'{', 1},
            {eol, 1},
            {string_lit, 2, "42"},
            {eol, 2},
            {'}', 3},
            {'catch', 3},
            {'{', 3},
            {eol, 3},
            {match, 4},
            {atom_lit, 4, first},
            {'->', 4},
            {eol, 4},
            {atom_lit, 5, first},
            {eol, 5},
            {match, 6},
            {atom_lit, 6, second},
            {'->', 6},
            {eol, 6},
            {atom_lit, 7, second},
            {eol, 7},
            {'}', 8},
            {'after', 8},
            {'{', 8},
            {eol, 8},
            {identifier, 9, "cleanup"},
            {'(', 9},
            {')', 9},
            {eol, 9},
            {'}', 10},
            {eol, 10}
        ],
        Tokens
    ).
