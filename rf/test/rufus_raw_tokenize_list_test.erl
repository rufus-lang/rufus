-module(rufus_raw_tokenize_list_test).

-include_lib("eunit/include/eunit.hrl").

string_with_function_returning_a_list_of_bool_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("func False() list[bool] { list[bool]{false} }"),
    ?assertEqual([
        {func, 1},
        {identifier, 1, "False"},
        {'(', 1},
        {')', 1},
        {list, 1},
        {'[', 1},
        {bool, 1},
        {']', 1},
        {'{', 1},
        {list, 1},
        {'[', 1},
        {bool, 1},
        {']', 1},
        {'{', 1},
        {bool_lit, 1, false},
        {'}', 1},
        {'}', 1}
    ], Tokens).

string_with_multiline_function_returning_a_list_of_bool_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("
    func False() list[bool] {
        list[bool]{false}
    }"
    ),
    ?assertEqual([
        {eol, 1},
        {func, 2},
        {identifier, 2, "False"},
        {'(', 2},
        {')', 2},
        {list, 2},
        {'[', 2},
        {bool, 2},
        {']', 2},
        {'{', 2},
        {eol, 2},
        {list, 3},
        {'[', 3},
        {bool, 3},
        {']', 3},
        {'{', 3},
        {bool_lit, 3, false},
        {'}',3},
        {eol, 3},
        {'}', 4}
    ], Tokens).

string_with_multiline_function_returning_a_cons_of_int_with_an_empty_tail_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("
    func Numbers() list[int] {
        list[int]{1|[]}
    }"
    ),
    ?assertEqual([
        {eol, 1},
        {func, 2},
        {identifier, 2, "Numbers"},
        {'(', 2},
        {')', 2},
        {list, 2},
        {'[', 2},
        {int, 2},
        {']', 2},
        {'{', 2},
        {eol, 2},
        {list, 3},
        {'[', 3},
        {int, 3},
        {']', 3},
        {'{', 3},
        {int_lit, 3, 1},
        {cons, 3},
        {'[', 3},
        {']', 3},
        {'}', 3},
        {eol, 3},
        {'}', 4}
    ], Tokens).

string_with_multiline_function_returning_a_cons_of_int_with_a_nonempty_tail_test() ->
    {ok, Tokens, _} = rufus_raw_tokenize:string("
    func Numbers() list[int] {
        list[int]{1|[2]}
    }"
    ),
    ?assertEqual([
        {eol, 1},
        {func, 2},
        {identifier, 2, "Numbers"},
        {'(', 2},
        {')', 2},
        {list, 2},
        {'[', 2},
        {int, 2},
        {']', 2},
        {'{', 2},
        {eol, 2},
        {list, 3},
        {'[', 3},
        {int, 3},
        {']', 3},
        {'{', 3},
        {int_lit, 3, 1},
        {cons, 3},
        {'[', 3},
        {int_lit, 3, 2},
        {']', 3},
        {'}', 3},
        {eol, 3},
        {'}', 4}
    ], Tokens).
