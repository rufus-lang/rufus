-module(rufus_raw_tokenize_literal_test).

-include_lib("eunit/include/eunit.hrl").

string_with_alphabetic_characters_in_string_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string(
        "\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\""
    ),
    ?assertEqual([{string_lit, 1, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"}], Tokens).

string_with_numeric_characters_in_string_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("\"0123456789\""),
    ?assertEqual([{string_lit, 1, "0123456789"}], Tokens).

string_with_whitespace_characters_in_string_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("\" 	\""),
    ?assertEqual([{string_lit, 1, " \t"}], Tokens).

string_with_punctuation_characters_in_string_literal_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("\"!@#$%^&*()-+_={}[];':,<>.?|/~`\""),
    ?assertEqual([{string_lit, 1, "!@#$%^&*()-+_={}[];':,<>.?|/~`"}], Tokens).
