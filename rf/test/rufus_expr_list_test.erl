-module(rufus_expr_list_test).

-include_lib("eunit/include/eunit.hrl").

typecheck_and_annotate_with_function_returning_a_list_of_one_int_test() ->
    RufusText = "
    module example
    func Numbers() list[int] { [10] }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{list_lit, #{elements => [{int_lit, #{line => 3,
                                                                         spec => 10,
                                                                         type => {type, #{line => 3,
                                                                                          source => inferred,
                                                                                          spec => int}}}}],
                                                line => 3,
                                                type => {type, #{collection_type => list,
                                                                 element_type => {type, #{line => 3,
                                                                                          source => inferred,
                                                                                          spec => int}},
                                                                 line => 3,
                                                                 source => rufus_text,
                                                                 spec => 'list[int]'}}}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{collection_type => list,
                                                 element_type => {type, #{line => 3,
                                                                          source => rufus_text,
                                                                          spec => int}},
                                                 line => 3,
                                                 source => rufus_text,
                                                 spec => 'list[int]'}},
                         spec => 'Numbers'}}],
    ?assertEqual(Expected, AnnotatedForms).
