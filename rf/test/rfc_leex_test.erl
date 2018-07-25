-module(rfc_leex_test).

-include_lib("eunit/include/eunit.hrl").

string_test() ->
    {ok, Tokens, Line} = rfc_leex:string("package math"),
    [{package, Line, "package"}, {identifier, Line, "math"}] = Tokens,
    1 = Line.
