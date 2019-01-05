-module(rufus_compile_erlang_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions returning a literal value for primitive types

forms_for_function_returning_a_bool_test() ->
    RufusText = "
    package example
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

forms_for_function_returning_a_float_test() ->
    RufusText = "
    package example
    func Pi() float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    FloatExpr = {float, 3, 3.14159265359},
    BoxedFloatExpr = {tuple, 3, [{atom, 3, float}, FloatExpr]},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Pi', 0}]},
        {function, 3, 'Pi', 0, [{clause, 3, [], [], [BoxedFloatExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_an_int_test() ->
    RufusText = "
    package example
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    IntegerExpr = {integer, 3, 42},
    BoxedIntegerExpr = {tuple, 3, [{atom, 3, int}, IntegerExpr]},
    Expected = [
        {attribute, 2, module, example},
        {attribute, 3, export, [{'Number', 0}]},
        {function, 3, 'Number', 0, [{clause, 3, [], [], [BoxedIntegerExpr]}]}
    ],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_returning_a_string_test() ->
    RufusText = "
    package example
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

%% Arity-1 functions using an argument

forms_for_function_taking_a_bool_and_returning_a_bool_test() ->
    RufusText = "
    package example
    func MaybeEcho(n bool) bool { true }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'MaybeEcho', 1}]},
                  {function, 3, 'MaybeEcho', 1,
                      [{clause, 3,
                           [{tuple, 3, [{atom, 3, bool}, {var, 3, '_n'}]}],
                           [],
                           [{tuple, 3, [{atom, 3, bool}, {atom, 3, true}]}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_float_and_returning_a_float_test() ->
    RufusText = "
    package example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'MaybeEcho', 1}]},
                  {function, 3, 'MaybeEcho', 1,
                      [{clause, 3,
                           [{tuple, 3, [{atom, 3, float}, {var, 3, '_n'}]}],
                           [],
                           [{tuple, 3, [{atom, 3, float}, {float, 3, 3.14159265359}]}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_an_int_and_returning_an_int_test() ->
    RufusText = "
    package example
    func MaybeEcho(n int) int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'MaybeEcho', 1}]},
                  {function, 3, 'MaybeEcho', 1,
                      [{clause, 3,
                           [{tuple, 3, [{atom, 3, int}, {var, 3, '_n'}]}],
                           [],
                           [{tuple, 3, [{atom, 3, int}, {integer, 3, 42}]}]}]}],
    ?assertEqual(Expected, ErlangForms).

forms_for_function_taking_a_string_and_returning_a_string_test() ->
    RufusText = "
    package example
    func MaybeEcho(n string) string { \"Hello\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, ErlangForms} = rufus_compile_erlang:forms(Forms),
    StringExpr = {bin_element, 3, {string, 3, "Hello"}, default, default},
    Expected = [{attribute, 2, module, example},
                  {attribute, 3, export, [{'MaybeEcho', 1}]},
                  {function, 3, 'MaybeEcho', 1,
                      [{clause, 3,
                           [{tuple, 3, [{atom, 3, string}, {var, 3, '_n'}]}],
                           [],
                           [{tuple, 3, [{atom, 3, string}, {bin,3, [StringExpr]}]}]}]}],
    ?assertEqual(Expected, ErlangForms).
