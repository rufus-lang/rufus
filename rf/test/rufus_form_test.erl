-module(rufus_form_test).

-include_lib("eunit/include/eunit.hrl").

make_inferred_type_test() ->
    ?assertEqual({type, #{spec => int, line => 4}}, rufus_form:make_user_specified_type(int, 4)).

make_user_specified_type_test() ->
    ?assertEqual({type, #{spec => float, line => 37}}, rufus_form:make_user_specified_type(float, 37)).
