-module(rufus_raw_tokenize_import_test).

-include_lib("eunit/include/eunit.hrl").

string_with_import_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("import \"bar\""),
    ?assertEqual([
        {import, 1},
        {string_lit, 1, "bar"}
], Tokens).
