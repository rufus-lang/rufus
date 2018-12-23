-module(rufus_erlang_compiler_test).

-include_lib("eunit/include/eunit.hrl").

forms_for_function_returning_an_float_test() ->
    RufusText = "
    package example
    func Pi() float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang_compiler:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [{clause, 3, [], [], [{float, 3, 3.14159265359}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_an_int_test() ->
    RufusText = "
    package example
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang_compiler:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Number', 0}]},
        {function, 3, 'Number', 0, [{clause, 3, [], [], [{integer, 3, 42}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).
