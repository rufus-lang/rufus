-module(rufus_parse_list_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_returning_empty_list_of_ints_test() ->
    RufusText = "
    module example
    func EmptyNumbers() list[int] { [] }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{list_lit, #{elements => [],
                                                line => 3}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{collection_type => list,
                                                 element_type => {type, #{line => 3,
                                                                          source => rufus_text,
                                                                          spec => int}},
                                                 line => 3,
                                                 source => rufus_text,
                                                 spec => 'list[int]'}},
                         spec => 'EmptyNumbers'}}],
    ?assertEqual(Expected, Forms).

parse_function_returning_list_of_int_with_one_element_test() ->
    RufusText = "
    module example
    func OneNumber() list[int] { [10] }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),

    Expected =

    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{list_lit, #{elements => [{int_lit, #{line => 3,
                                                                         spec => 10,
                                                                         type => {type, #{line => 3,
                                                                                          source => inferred,
                                                                                          spec => int}}}}],
                                                line => 3}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{collection_type => list,
                                                 element_type => {type, #{line => 3,
                                                                          source => rufus_text,
                                                                          spec => int}},
                                                 line => 3,
                                                 source => rufus_text,
                                                 spec => 'list[int]'}},
                         spec => 'OneNumber'}}],
    ?assertEqual(Expected, Forms).

parse_function_returning_list_of_many_ints_test() ->
    RufusText = "
    module example
    func ManyNumbers() list[int] { [10, 2] }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{list_lit, #{elements => [{int_lit, #{line => 3,
                                                                         spec => 10,
                                                                         type => {type, #{line => 3,
                                                                                          source => inferred,
                                                                                          spec => int}}}},
                                                             {int_lit, #{line => 3,
                                                                         spec => 2,
                                                                         type => {type, #{line => 3,
                                                                                          source => inferred,
                                                                                          spec => int}}}}],
                                                line => 3}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{collection_type => list,
                                                 element_type => {type, #{line => 3,
                                                                          source => rufus_text,
                                                                          spec => int}},
                                                 line => 3,
                                                 source => rufus_text,
                                                 spec => 'list[int]'}},
                         spec => 'ManyNumbers'}}],
    ?assertEqual(Expected, Forms).

parse_function_returning_nested_list_of_list_of_ints_test() ->
    RufusText = "
    module example
    func NestedNumbers() list[list[int]] { [[1], [2, 3]] }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{list_lit, #{elements => [{list_lit, #{elements => [{int_lit, #{line => 3,
                                                                                                   spec => 1,
                                                                                                   type => {type, #{line => 3,
                                                                                                                    source => inferred,
                                                                                                                    spec => int}}}}],
                                                                          line => 3}},
                                                             {list_lit, #{elements => [{int_lit, #{line => 3,
                                                                                                   spec => 2,
                                                                                                   type => {type, #{line => 3,
                                                                                                                    source => inferred,
                                                                                                                    spec => int}}}},
                                                                                       {int_lit, #{line => 3,
                                                                                                   spec => 3,
                                                                                                   type => {type, #{line => 3,
                                                                                                                    source => inferred,
                                                                                                                    spec => int}}}}],
                                                                          line => 3}}],
                                                line => 3}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{collection_type => list,
                                                 element_type => {type, #{collection_type => list,
                                                                          element_type => {type, #{line => 3,
                                                                                                   source => rufus_text,
                                                                                                   spec => int}},
                                                                          line => 3,
                                                                          source => rufus_text,
                                                                          spec => 'list[int]'}},
                                                 line => 3,
                                                 source => rufus_text,
                                                 spec => 'list[list[int]]'}},
                         spec => 'NestedNumbers'}}],
    ?assertEqual(Expected, Forms).
