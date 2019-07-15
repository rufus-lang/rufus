-module(rufus_compile_erlang_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a literal value for scalar types

forms_for_function_returning_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping() atom { :pong }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    AtomExpr = {atom, 3, pong},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Ping', 0}]},
        {function, 3, 'Ping', 0, [{clause, 3, [], [], [AtomExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func False() bool { false }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    BoolExpr = {atom, 3, false},
    BoxedBoolExpr = {tuple, 3, [{atom, 3, bool}, BoolExpr]},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'False', 0}]},
        {function, 3, 'False', 0, [{clause, 3, [], [], [BoxedBoolExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func Pi() float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [{clause, 3, [], [], [{float, 3, 3.14159265359}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Number', 0}]},
        {function, 3, 'Number', 0, [{clause, 3, [], [], [{integer, 3, 42}]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func Greeting() string { \"Hello\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    StringExpr = {bin, 3, [{bin_element, 3, {string, 3, "Hello"}, default, default}]},
    BoxedStringExpr = {tuple, 3, [{atom, 3, string}, StringExpr]},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Greeting', 0}]},
        {function, 3, 'Greeting', 0, [{clause, 3, [], [], [BoxedStringExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

%% Arity-0 functions with multiple function expressions

forms_for_function_with_multiple_expressions_test() ->
    RufusText = "
    module example
    func Multiple() atom {
        42
        :fortytwo
    }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Exprs = [{integer, 4, 42}, {atom, 5, fortytwo}],
    Expected = [{attribute, 2, module, example},
                {attribute, 3, export, [{'Multiple', 0}]},
                {function, 3, 'Multiple', 0, [{clause, 3, [], [], Exprs}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_with_multiple_expressions_with_blank_lines_test() ->
    RufusText = "
    module example
    func Multiple() atom {
        42

        :fortytwo
    }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Exprs = [{integer, 4, 42}, {atom, 6, fortytwo}],
    Expected = [{attribute, 2, module, example},
                {attribute, 3, export, [{'Multiple', 0}]},
                {function, 3, 'Multiple', 0, [{clause, 3, [], [], Exprs}]}],
    ?assertEqual(Expected, ErlangForms).

%% Arity-1 functions taking an unused argument

forms_for_function_taking_an_unused_atom_and_returning_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping(m atom) atom { :pong }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [{attribute, 2, module, example},
                {attribute, 3, export, [{'Ping', 1}]},
                {function, 3, 'Ping', 1,
                 [{clause, 3,
                   [{var, 3, m}],
                   [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_atom}}, [{var, 3, m}]}]],
                   [{atom, 3, pong}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_bool_and_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(b bool) bool { true }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'MaybeEcho', 1}]},
                  {function, 3, 'MaybeEcho', 1,
                      [{clause, 3,
                           [{tuple, 3, [{atom, 3, bool}, {var, 3, b}]}],
                           [],
                           [{tuple, 3, [{atom, 3, bool}, {atom, 3, true}]}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_float_and_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'MaybeEcho', 1}]},
                  {function, 3, 'MaybeEcho', 1,
                      [{clause, 3,
                           [{var, 3, n}],
                           [[{call,3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, n}]}]],
                           [{float, 3, 3.14159265359}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_int_and_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n int) int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'MaybeEcho', 1}]},
                  {function, 3, 'MaybeEcho', 1,
                      [{clause, 3,
                           [{var, 3, n}],
                           [[{call,3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, n}]}]],
                           [{integer, 3, 42}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_unused_string_and_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(s string) string { \"Hello\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    StringExpr = {bin_element, 3, {string, 3, "Hello"}, default, default},
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'MaybeEcho', 1}]},
                  {function, 3, 'MaybeEcho', 1,
                      [{clause, 3,
                           [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}],
                           [],
                           [{tuple, 3, [{atom, 3, string}, {bin, 3, [StringExpr]}]}]}]}],
    ?assertEqual(Expected, ErlangForms).

%% Arity-1 functions taking and using an argument

forms_for_function_taking_an_atom_and_returning_an_atom_test() ->
    RufusText = "
    module example
    func Echo(m atom) atom { m }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_annotate_locals:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
    Expected = [{attribute, 2, module, example},
                {attribute, 3, export, [{'Echo', 1}]},
                {function, 3, 'Echo', 1,
                 [{clause, 3,
                   [{var, 3, m}],
                   [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_atom}}, [{var, 3, m}]}]],
                   [{var, 3, m}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_bool_and_returning_a_bool_test() ->
    RufusText = "
    module example
    func Echo(b bool) bool { b }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_annotate_locals:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'Echo', 1}]},
                  {function, 3, 'Echo', 1,
                      [{clause, 3,
                           [{tuple, 3, [{atom, 3, bool}, {var, 3, b}]}],
                           [],
                           [{tuple, 3, [{atom, 3, bool}, {var, 3, b}]}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_float_and_returning_a_float_test() ->
    RufusText = "
    module example
    func Echo(n float) float { n }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_annotate_locals:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'Echo', 1}]},
                  {function, 3, 'Echo', 1,
                      [{clause, 3,
                           [{var, 3, n}],
                           [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_float}}, [{var, 3, n}]}]],
                           [{var, 3, n}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_int_and_returning_an_int_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_annotate_locals:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'Echo', 1}]},
                  {function, 3, 'Echo', 1,
                      [{clause, 3,
                           [{var, 3, n}],
                           [[{call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, is_integer}}, [{var, 3, n}]}]],
                           [{var, 3, n}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_string_and_returning_a_string_test() ->
    RufusText = "
    module example
    func Echo(s string) string { s }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_annotate_locals:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'Echo', 1}]},
                  {function, 3, 'Echo', 1,
                      [{clause, 3,
                           [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}],
                           [],
                           [{tuple, 3, [{atom, 3, string}, {var, 3, s}]}]}]}],
    ?assertEqual(Expected, ErlangForms).

%% Arity-0 functions returning a sum of literal values for scalar types

forms_for_function_returning_a_sum_of_int_literals_test() ->
    RufusText = "
    module example
    func FortyTwo() int { 19 + 23 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_typecheck_binary_op:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_typecheck_binary_op:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_typecheck_binary_op:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_typecheck_binary_op:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    BinaryOpExpr = {op, 3, '/', {float, 3, 5.5}, {float, 3, 2.0}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'TwoPointSevenFive', 0}]},
        {function, 3, 'TwoPointSevenFive', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_remainder_of_int_literals_test() ->
    RufusText = "
    module example
    func Six() int { 27 % 7 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
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
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_typecheck_binary_op:forms(Forms),
    {ok, ErlangForms} = rufus_compile_erlang:forms(AnnotatedForms),
    LeftExpr = {op, 3, 'rem', {integer, 3, 100}, {integer, 3, 13}},
    BinaryOpExpr = {op, 3, 'rem', LeftExpr, {integer, 3, 5}},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Four', 0}]},
        {function, 3, 'Four', 0, [{clause, 3, [], [], [BinaryOpExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).
