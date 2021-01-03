-module(rufus_raw_tokenize_throw_test).

-include_lib("eunit/include/eunit.hrl").

string_with_throw_expression_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("throw :kaboom\n"),
    ?assertEqual(
        [
            {throw, 1},
            {atom_lit, 1, kaboom},
            {eol, 1}
        ],
        Tokens
    ).
