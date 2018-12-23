-module(rufus_parse_package_test).

-include_lib("eunit/include/eunit.hrl").

empty_package_test() ->
    {ok, Tokens, _} = rufus_scan:string("package empty"),
    {ok, AST} = rufus_parse:parse(Tokens),
    [
     {package, 1, "empty"}
    ] = AST.

import_test() ->
    {ok, Tokens, _} = rufus_scan:string("
package foo
import \"bar\"
"),
    {ok, AST} = rufus_parse:parse(Tokens),
    [
     {package, 2, "foo"},
     {import, 3, "bar"}
    ] = AST.
