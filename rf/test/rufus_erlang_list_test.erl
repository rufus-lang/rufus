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

forms_for_function_returning_a_cons_literal_with_literal_pair_values_test() ->
    RufusText = "
    module example
    func Numbers() list[int] { list[int]{1|{2}} }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Numbers', 0}]},
        {function, 3, 'Numbers', 0, [{clause, 3, [], [], [{cons, 3, {integer, 3, 1},
                                                           {cons, 3, {integer, 3, 2},
                                                                     {nil, 3}}}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_cons_literal_with_multiple_literal_pair_values_test() ->
    RufusText = "
    module example
    func Numbers() list[int] { list[int]{1|{2, 3, 4}} }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Numbers', 0}]},
        {function, 3, 'Numbers', 0, [{clause, 3, [], [],
                                      [{cons, 3, {integer, 3, 1},
                                        {cons, 3, {integer, 3, 2},
                                         {cons, 3, {integer, 3, 3},
                                          {cons, 3, {integer, 3, 4},
                                           {nil, 3}}}}}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_cons_literal_with_variable_pair_values_test() ->
    RufusText = "
    module example
    func Numbers() list[int] {
        head = 1
        tail = list[int]{2, 3, 4}
        list[int]{head|tail}
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Numbers', 0}]},
        {function, 3, 'Numbers', 0,
         [{clause, 3, [], [],
           [{match, 4, {var, 4, head}, {integer, 4, 1}},
            {match, 5,
             {var, 5, tail},
             {cons, 5, {integer, 5, 2},
              {cons, 5, {integer, 5, 3},
               {cons, 5, {integer, 5, 4},
                {nil, 5}}}}},
            {cons, 6,
             {var, 6, head},
             {var, 6, tail}}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).
