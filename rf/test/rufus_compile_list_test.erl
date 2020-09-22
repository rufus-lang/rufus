-module(rufus_compile_list_test).

-include_lib("eunit/include/eunit.hrl").

eval_for_function_returning_an_empty_list_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Empty() list[int] { list[int]{} }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([], example:'Empty'()).

eval_for_function_returning_a_list_with_a_single_element_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{7} }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([7], example:'Numbers'()).

eval_for_function_returning_a_list_with_two_elements_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{7, 918} }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([7, 918], example:'Numbers'()).

eval_for_function_returning_a_list_of_lists_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[list[int]] {\n"
        "        list[list[int]]{\n"
        "            list[int]{7},\n"
        "            list[int]{918, 23},\n"
        "        }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([[7], [918, 23]], example:'Numbers'()).

eval_for_function_returning_a_list_with_expressions_as_elements_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Three() int { 3 }\n"
        "    func Numbers() list[int] {\n"
        "        list[int]{\n"
        "            3 + 4,\n"
        "            5 = 5,\n"
        "            Three(),\n"
        "        }\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([7, 5, 3], example:'Numbers'()).

eval_for_function_returning_a_cons_literal_with_literal_pair_values_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{1|{2}} }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([1, 2], example:'Numbers'()).

eval_for_function_returning_a_cons_literal_with_multiple_literal_pair_values_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] { list[int]{1|{2, 3, 4}} }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([1, 2, 3, 4], example:'Numbers'()).

eval_for_function_returning_a_cons_literal_with_variable_pair_values_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Numbers() list[int] {\n"
        "        head = 1\n"
        "        tail = list[int]{2, 3, 4}\n"
        "        list[int]{head|tail}\n"
        "    }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([1, 2, 3, 4], example:'Numbers'()).

eval_for_function_taking_a_list_lit_form_and_returning_it_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Echo(numbers list[int]) list[int] { numbers }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([1, 2, 3, 4], example:'Echo'([1, 2, 3, 4])).

eval_for_function_that_prepends_a_number_to_a_list_test() ->
    RufusText =
        "\n"
        "    module example\n"
        "    func Prepend(n int, numbers list[int]) list[int] { list[int]{n|numbers} }\n"
        "    ",
    Result = rufus_compile:eval(RufusText),
    ?assertEqual({ok, example}, Result),
    ?assertEqual([1, 2, 3, 4], example:'Prepend'(1, [2, 3, 4])).
