-module(rufus_scope_locals_test).

-include_lib("eunit/include/eunit.hrl").

forms_with_empty_module_test() ->
    RufusText = "module empty",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_scope_locals:forms(Forms)).

forms_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_scope_locals:forms(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{args => [],
                 exprs => [{int_lit, #{line => 3, locals => #{}, spec => 42,
                                       type => {type, #{line => 3, spec => int}}}}],
                 line => 3,
                 return_type => {type, #{line => 3, spec => int}},
                 spec => 'Number'}
    }],
    ?assertEqual(Expected, AnnotatedForms).

%% Arity-1 functions taking an argument and returning a literal

forms_for_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(b bool) bool { true }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_scope_locals:forms(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func,
         #{args => [{arg, #{line => 3,
                            spec => b,
                            type => {type, #{line => 3, spec => bool}}}}],
           exprs => [{bool_lit, #{line => 3,
                                   locals => #{b => {type, #{line => 3, spec => bool}}},
                                   spec => true,
                                   type => {type, #{line => 3, spec => bool}}}}],
           line => 3,
           return_type => {type, #{line => 3, spec => bool}},
           spec => 'MaybeEcho'}
        }
    ],
    ?assertEqual(Expected, AnnotatedForms).

forms_for_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_scope_locals:forms(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func,
         #{args => [{arg, #{line => 3,
                            spec => n,
                            type => {type, #{line => 3, spec => float}}}}],
           exprs => [{float_lit, #{line => 3,
                                   locals => #{n => {type, #{line => 3, spec => float}}},
                                   spec => 3.14159265359,
                                   type => {type, #{line => 3, spec => float}}}}],
           line => 3,
           return_type => {type, #{line => 3, spec => float}},
           spec => 'MaybeEcho'}
        }
    ],
    ?assertEqual(Expected, AnnotatedForms).

forms_for_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n int) int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_scope_locals:forms(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func,
         #{args => [{arg, #{line => 3,
                            spec => n,
                            type => {type, #{line => 3, spec => int}}}}],
           exprs => [{int_lit, #{line => 3,
                                 locals => #{n => {type, #{line => 3, spec => int}}},
                                 spec => 42,
                                 type => {type, #{line => 3, spec => int}}}}],
           line => 3,
           return_type => {type, #{line => 3, spec => int}},
           spec => 'MaybeEcho'}
        }
    ],
    ?assertEqual(Expected, AnnotatedForms).

forms_for_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(s string) string { \"Hello\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_scope_locals:forms(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func,
         #{args => [{arg, #{line => 3,
                            spec => s,
                            type => {type, #{line => 3, spec => string}}}}],
           exprs => [{string_lit, #{line => 3,
                                    locals => #{s => {type, #{line => 3, spec => string}}},
                                    spec => <<"Hello">>,
                                    type => {type, #{line => 3, spec => string}}}}],
           line => 3,
           return_type => {type, #{line => 3, spec => string}},
           spec => 'MaybeEcho'}
        }
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Arity-1 functions taking and returning an argument

forms_for_function_taking_a_bool_and_returning_it_test() ->
    RufusText = "
    module example
    func MaybeEcho(b bool) bool { b }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_scope_locals:forms(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func,
         #{args => [{arg, #{line => 3,
                            spec => b,
                            type => {type, #{line => 3, spec => bool}}}}],
           exprs =>
                    [{identifier, #{line => 3,
                                    locals => #{b => {type, #{line => 3, spec => bool}}},
                                    spec => b}}],
           line => 3,
           return_type => {type, #{line => 3, spec => bool}},
           spec => 'MaybeEcho'}
        }
    ],
    ?assertEqual(Expected, AnnotatedForms).
