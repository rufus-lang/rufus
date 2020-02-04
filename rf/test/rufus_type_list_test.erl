-module(rufus_type_list_test).

-include_lib("eunit/include/eunit.hrl").

resolve_list_with_one_element_test() ->
    Element = rufus_form:make_literal(int, 42, 3),
    IntTypeForm = rufus_form:make_type(int, 3),
    Form = rufus_form:make_literal(list, IntTypeForm, [Element], 3),
    Expected = rufus_form:make_type(list, IntTypeForm, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form)).

resolve_list_with_mismatched_element_type_test() ->
    Element = rufus_form:make_literal(float, 42.0, 3),
    IntTypeForm = rufus_form:make_type(int, 3),
    Form = rufus_form:make_literal(list, IntTypeForm, [Element], 3),
    Data = #{form => {list_lit, #{elements => [{float_lit, #{line => 3,
                                                             spec => 42.0,
                                                             type => {type, #{line => 3,
                                                                              source => inferred,
                                                                              spec => float}}}}],
                                  line => 3,
                                  type => {type, #{collection_type => list,
                                                   element_type => {type, #{line => 3,
                                                                            source => rufus_text,
                                                                            spec => int}},
                                                   line => 3,
                                                   source => rufus_text,
                                                   spec => 'list[int]'}}}}},
    ?assertEqual({error, unexpected_element_type, Data}, rufus_type:resolve(#{}, Form)).
