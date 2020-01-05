-module(rufus_erlang_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a sum of literal values for scalar types

forms_for_function_returning_a_sum_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 19 + 23 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, '+', {integer, 3, 19}, {integer, 3, 23}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'FortyTwo', 0}]},
        {function, 3, 'FortyTwo', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_sum_of_three_int_literals_test() ->
    RufusText = "
    module example
    func FiftyNine() int { 19 + 23 + 17 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    LeftExpr = {op, 3, '+', {integer, 3, 19}, {integer, 3, 23}},
    BinaryOpExpr = {op, 3, '+', LeftExpr, {integer, 3, 17}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'FiftyNine', 0}]},
        {function, 3, 'FiftyNine', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_sum_of_float_literals_test() ->
    RufusText = "
    module example
    func Pi() float { 1.0 + 2.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, '+', {float, 3, 1.0}, {float, 3, 2.14159265359}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-0 functions returning a difference of literal values for scalar types

forms_for_function_returning_a_difference_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 55 - 13 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, '-', {integer, 3, 55}, {integer, 3, 13}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'FortyTwo', 0}]},
        {function, 3, 'FortyTwo', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_difference_of_three_int_literals_test() ->
    RufusText = "
    module example
    func ThirteenThirtyFive() int { 1500 - 150 - 15 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    LeftExpr = {op, 3, '-', {integer, 3, 1500}, {integer, 3, 150}},
    BinaryOpExpr = {op, 3, '-', LeftExpr, {integer, 3, 15}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'ThirteenThirtyFive', 0}]},
        {function, 3, 'ThirteenThirtyFive', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_difference_of_float_literals_test() ->
    RufusText = "
    module example
    func Pi() float { 4.14159265359 - 1.0 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, '-', {float, 3, 4.14159265359}, {float, 3, 1.0}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-0 functions returning a product of literal values for scalar types

forms_for_function_returning_a_product_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 3 * 14 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, '*', {integer, 3, 3}, {integer, 3, 14}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'FortyTwo', 0}]},
        {function, 3, 'FortyTwo', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_product_of_three_int_literals_test() ->
    RufusText = "
    module example
    func ThirteenThirtyFive() int { 3 * 5 * 89 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    LeftExpr = {op, 3, '*', {integer, 3, 3}, {integer, 3, 5}},
    BinaryOpExpr = {op, 3, '*', LeftExpr, {integer, 3, 89}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'ThirteenThirtyFive', 0}]},
        {function, 3, 'ThirteenThirtyFive', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_product_of_float_literals_test() ->
    RufusText = "
    module example
    func Pi() float { 1.0 * 3.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, '*', {float, 3, 1.0}, {float, 3, 3.14159265359}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-0 functions returning a division of literal values for scalar types

forms_for_function_returning_a_division_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 84 / 2 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, 'div', {integer, 3, 84}, {integer, 3, 2}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'FortyTwo', 0}]},
        {function, 3, 'FortyTwo', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_division_of_three_int_literals_test() ->
    RufusText = "
    module example
    func Five() int { 100 / 10 / 2 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    LeftExpr = {op, 3, 'div', {integer, 3, 100}, {integer, 3, 10}},
    BinaryOpExpr = {op, 3, 'div', LeftExpr, {integer, 3, 2}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Five', 0}]},
        {function, 3, 'Five', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_division_of_float_literals_test() ->
    RufusText = "
    module example
    func TwoPointSevenFive() float { 5.5 / 2.0 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, '/', {float, 3, 5.5}, {float, 3, 2.0}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'TwoPointSevenFive', 0}]},
        {function, 3, 'TwoPointSevenFive', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-0 functions returning a remainder after dividing literal values

forms_for_function_returning_a_remainder_of_int_literals_test() ->
    RufusText = "
    module example
    func Six() int { 27 % 7 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, 'rem', {integer, 3, 27}, {integer, 3, 7}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Six', 0}]},
        {function, 3, 'Six', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_remainder_of_three_int_literals_test() ->
    RufusText = "
    module example
    func Four() int { 100 % 13 % 5 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    LeftExpr = {op, 3, 'rem', {integer, 3, 100}, {integer, 3, 13}},
    BinaryOpExpr = {op, 3, 'rem', LeftExpr, {integer, 3, 5}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Four', 0}]},
        {function, 3, 'Four', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-0 functions returning the result of a boolean operation

forms_for_function_returning_a_boolean_from_an_and_operation_test() ->
    RufusText = "
    module example
    func Falsy() bool { true and false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, 'andalso', {atom, 3, true}, {atom, 3, false}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Falsy', 0}]},
        {function, 3, 'Falsy', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_boolean_from_an_and_operation_with_a_call_operand_test() ->
    RufusText = "
    module example
    func False() bool { false }
    func Falsy() bool { true and False() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 4, 'andalso',  {atom, 4, true}, {call, 4, {atom, 4, 'False'}, []}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 4, export, [{'Falsy', 0}]},
        {attribute, 3, export, [{'False', 0}]},
        {function, 3, 'False', 0, [{clause, 3, [], [], [{atom, 3, false}]}]},
        {function, 4, 'Falsy', 0, [{clause, 4, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_boolean_from_an_or_operation_test() ->
    RufusText = "
    module example
    func Truthy() bool { true or false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, 'orelse', {atom, 3, true}, {atom, 3, false}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Truthy', 0}]},
        {function, 3, 'Truthy', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_boolean_from_an_or_operation_with_a_call_test() ->
    RufusText = "
    module example
    func True() bool { true }
    func Truthy() bool { True() or false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    {ok, ErlangForms} = rufus_erlang:forms(AnnotatedForms),
    BinaryOpExpr = {op, 4, 'orelse', {call, 4, {atom, 4, 'True'}, []}, {atom, 4, false}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 4, export, [{'Truthy', 0}]},
        {attribute, 3, export, [{'True', 0}]},
        {function, 3, 'True', 0, [{clause, 3, [], [], [{atom, 3, true}]}]},
        {function, 4, 'Truthy', 0, [{clause, 4, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).
