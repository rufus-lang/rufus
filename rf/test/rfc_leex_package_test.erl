-module(rfc_leex_package_test).

-include_lib("eunit/include/eunit.hrl").

empty_package_test() ->
    {ok, Tokens, _} = rfc_leex:string("package empty"),
    [{package, 1, "package"}, {identifier, 1, "empty"}] = Tokens.

import_test() ->
    {ok, Tokens, _} = rfc_leex:string("
package foo
import \"bar\""),
    [
     {package, 2, "package"}, {identifier, 2, "foo"},
     {import, 3, "import"}, {string, 3, "bar"}
    ] = Tokens.
