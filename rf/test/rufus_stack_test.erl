-module(rufus_stack_test).

-include_lib("eunit/include/eunit.hrl").

is_param_test() ->
    Type = rufus_form:make_type(bool, 81),
    Param = rufus_form:make_literal(bool, true, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form = rufus_form:make_func('True', [Param], Type, [Value], 81),
    ParamsForm = rufus_form:make_params(Form),
    Stack = [ParamsForm, Form],
    ?assert(rufus_stack:is_param(Stack)).

is_param_without_params_in_stack_test() ->
    Type = rufus_form:make_type(bool, 81),
    Param = rufus_form:make_literal(bool, true, 81),
    Value = rufus_form:make_literal(bool, true, 81),
    Form = rufus_form:make_func('True', [Param], Type, [Value], 81),
    Stack = [Form],
    ?assertNot(rufus_stack:is_param(Stack)).
