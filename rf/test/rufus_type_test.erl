-module(rufus_type_test).

-include_lib("eunit/include/eunit.hrl").

resolve_form_with_an_existing_type_form_test() ->
    Form = rufus_form:make_literal(bool, true, 7),
    Expected = {type, #{spec => bool, source => inferred, line => 7}},
    ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form)).
