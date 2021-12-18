-module(rufus_raw_tokenize_comment_test).

-include_lib("eunit/include/eunit.hrl").

string_with_comment_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("// rufus programming language"),
    ?assertEqual([{comment, 1, "// rufus programming language"}], Tokens).

string_with_multiline_comment_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "// The rufus programming language is\n"
        "// implemented in Erlang and runs on the BEAM\n"
    ),
    ?assertEqual(
        [
            {comment, 1, "// The rufus programming language is"},
            {eol, 1},
            {comment, 2, "// implemented in Erlang and runs on the BEAM"},
            {eol, 2}
        ],
        Tokens
    ).

string_with_end_of_line_comment_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "i = 1// index\n"
        "j = 2\n"
    ),
    ?assertEqual(
        [
            {identifier, 1, "i"},
            {'=', 1},
            {int_lit, 1, 1},
            {comment, 1, "// index"},
            {eol, 1},
            {identifier, 2, "j"},
            {'=', 2},
            {int_lit, 2, 2},
            {eol, 2}
        ],
        Tokens
    ).

string_with_commented_our_source_text_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("// func number() int { 42 }"),
    ?assertEqual([{comment, 1, "// func number() int { 42 }"}], Tokens).

string_with_commented_our_source_text_and_newline_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("// func number() int { 42 }\n"),
    ?assertEqual([{comment, 1, "// func number() int { 42 }"}, {eol, 1}], Tokens).

string_with_docstring_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "// Random returns a randomly generated number.\n"
        "func Random() int { 42 }\n"
    ),
    ?assertEqual(
        [
            {comment, 1, "// Random returns a randomly generated number."},
            {eol, 1},
            {func, 2},
            {identifier, 2, "Random"},
            {'(', 2},
            {')', 2},
            {int, 2},
            {'{', 2},
            {int_lit, 2, 42},
            {'}', 2},
            {eol, 2}
        ],
        Tokens
    ).
