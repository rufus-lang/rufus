-module(rufus_compile_list_test).

-include_lib("eunit/include/eunit.hrl").

eval_for_function_returning_an_empty_list_test() ->
    RufusText = "
    module example
    func Empty() list[int] { list[int]{} }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([], example:'Empty'()).

eval_for_function_returning_a_list_with_a_single_element_test() ->
    RufusText = "
    module example
    func Numbers() list[int] { list[int]{7} }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([7], example:'Numbers'()).

eval_for_function_returning_a_list_with_two_elements_test() ->
    RufusText = "
    module example
    func Numbers() list[int] { list[int]{7, 918} }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([7, 918], example:'Numbers'()).

eval_for_function_returning_a_list_of_lists_test() ->
    RufusText = "
    module example
    func Numbers() list[list[int]] {
        list[list[int]]{
            list[int]{7},
            list[int]{918, 23},
        }
    }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([[7], [918, 23]], example:'Numbers'()).

eval_for_function_returning_a_list_with_expressions_as_elements_test() ->
    RufusText = "
    module example
    func Three() int { 3 }
    func Numbers() list[int] {
        list[int]{
            3 + 4,
            5 = 5,
            Three(),
        }
    }
    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([7, 5, 3], example:'Numbers'()).
