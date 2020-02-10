-module(rufus_erlang_list_test).

-include_lib("eunit/include/eunit.hrl").

forms_for_function_returning_an_empty_list_lit_form_test() ->
    RufusText = "
    module example
    func Empty() list[int] { list[int]{} }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Empty', 0}]},
        {function, 3, 'Empty', 0, [{clause, 3, [], [], [{nil, 3}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_list_lit_form_with_a_single_item_test() ->
    RufusText = "
    module example
    func Empty() list[bool] { list[bool]{true} }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Empty', 0}]},
        {function, 3, 'Empty', 0, [{clause, 3, [], [], [{cons, 3, {atom, 3, true},
                                                                   {nil, 3}}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_list_lit_form_with_a_two_items_test() ->
    RufusText = "
    module example
    func Empty() list[float] { list[float]{3.1, 4.1} }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Empty', 0}]},
        {function, 3, 'Empty', 0, [{clause, 3, [], [], [{cons, 3, {float, 3, 3.1},
                                                                  {cons, 3, {float, 3, 4.1},
                                                                            {nil, 3}}}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).
