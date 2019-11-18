-module(rufus_expr_module_test).

-include_lib("eunit/include/eunit.hrl").

typecheck_and_annotate_with_empty_module_test() ->
    RufusText = "module empty",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_expr:typecheck_and_annotate(Forms)).
