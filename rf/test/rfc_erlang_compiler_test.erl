-module(rfc_erlang_compiler_test).

-include_lib("eunit/include/eunit.hrl").

forms_test() ->
    RufusText = "
    package example
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rfc_scan:string(RufusText),
    {ok, Forms} = rfc_parse:parse(Tokens),
    {ok, ErlangForms} = rfc_erlang_compiler:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Number',0}]},
        {function, 3, 'Number', 0, [{clause,3,[],[],[{integer,3,42}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).
