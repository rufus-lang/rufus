-module(rufus_type_list_test).

-include_lib("eunit/include/eunit.hrl").

resolve_list_with_one_element_test() ->
    RufusText = "
    module example
    func Random() list[int] { list[int]{42} }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_form:globals(Forms),
    Element = rufus_form:make_literal(int, 42, 3),
    IntTypeForm = rufus_form:make_type(int, 3),
    Form = rufus_form:make_literal(list, IntTypeForm, [Element], 3),
    Expected = rufus_form:make_type(list, IntTypeForm, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).
