-module(rufus_scan_package_test).

-include_lib("eunit/include/eunit.hrl").

empty_package_test() ->
    {ok, Tokens, _} = rufus_scan:string("package empty"),
    [
     {package, 1},
     {identifier, 1, "empty"}
    ] = Tokens.

import_test() ->
    {ok, Tokens, _} = rufus_scan:string("
     package foo
     import \"bar\"
    "),
    [
     {package, 2},
     {identifier, 2, "foo"},
     {import, 3},
     {string_lit, 3, "bar"}
    ] = Tokens.
