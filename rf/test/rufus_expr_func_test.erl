-module(rufus_expr_func_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_does_not_allow_locals_to_escape_function_scope_test() ->
    RufusText = "
    module example
    func Echo(n string) string {
        a = n
        a
    }
    func Broken() string { a }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_expr:typecheck_and_annotate(Forms),
    Data = #{form => {identifier, #{line => 7,
                                    locals => #{},
                                    spec => a}},
             globals => #{'Broken' => [{func, #{exprs => [{identifier, #{line => 7,
                                                                         spec => a}}],
                                                line => 7,
                                                params => [],
                                                return_type => {type, #{line => 7,
                                                                        source => rufus_text,
                                                                        spec => string}},
                                                spec => 'Broken'}}],
                          'Echo' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                                         spec => a}},
                                                                  line => 4,
                                                                  right => {identifier, #{line => 4,
                                                                                          spec => n}}}},
                                                        {identifier, #{line => 5,
                                                                       spec => a}}],
                                              line => 3,
                                              params => [{param, #{line => 3,
                                                                   spec => n,
                                                                   type => {type, #{line => 3,
                                                                                    source => rufus_text,
                                                                                    spec => string}}}}],
                                              return_type => {type, #{line => 3,
                                                                      source => rufus_text,
                                                                      spec => string}},
                                              spec => 'Echo'}}]},
             locals => #{}},
    ?assertEqual({error, unknown_identifier, Data}, Result).

typecheck_and_annotate_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2, spec => example}},
        {func, #{params => [],
                 exprs => [{int_lit, #{line => 3, spec => 42,
                                       type => {type, #{line => 3, spec => int, source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3, spec => int, source => rufus_text}},
                 spec => 'Number'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Arity-1 functions taking an argument and returning a literal

typecheck_and_annotate_for_function_taking_an_atom_and_returning_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping(m atom) atom { :pong }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => m,
                                      type => {type, #{line => 3,
                                                       spec => atom,
                                                       source => rufus_text}}}}],
                 exprs => [{atom_lit, #{line => 3,
                                        spec => pong,
                                        type => {type, #{line => 3,
                                                         spec => atom,
                                                         source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => atom,
                                         source => rufus_text}},
                 spec => 'Ping'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_bool_and_returning_a_bool_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(b bool) bool { true }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => b,
                                      type => {type, #{line => 3,
                                                       spec => bool,
                                                       source => rufus_text}}}}],
                 exprs => [{bool_lit, #{line => 3,
                                        spec => true,
                                        type => {type, #{line => 3,
                                                         spec => bool,
                                                         source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => bool,
                                         source => rufus_text}},
                 spec => 'MaybeEcho'}
        }
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_float_and_returning_a_float_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n float) float { 3.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => n,
                                      type => {type, #{line => 3,
                                                       spec => float,
                                                       source => rufus_text}}}}],
                 exprs => [{float_lit, #{line => 3,
                                         spec => 3.14159265359,
                                         type => {type, #{line => 3,
                                                          spec => float,
                                                          source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => float,
                                         source => rufus_text}},
                 spec => 'MaybeEcho'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_an_int_and_returning_an_int_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(n int) int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => n,
                                      type => {type, #{line => 3,
                                                       spec => int,
                                                       source => rufus_text}}}}],
                 exprs => [{int_lit, #{line => 3,
                                       spec => 42,
                                       type => {type, #{line => 3,
                                                        spec => int,
                                                        source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => int,
                                         source => rufus_text}},
                 spec => 'MaybeEcho'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_string_and_returning_a_string_literal_test() ->
    RufusText = "
    module example
    func MaybeEcho(s string) string { \"Hello\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => s,
                                      type => {type, #{line => 3,
                                                       spec => string,
                                                       source => rufus_text}}}}],
                 exprs => [{string_lit, #{line => 3,
                                          spec => <<"Hello">>,
                                          type => {type, #{line => 3,
                                                           spec => string,
                                                           source => inferred}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => string,
                                         source => rufus_text}},
                 spec => 'MaybeEcho'}}
    ],
    ?assertEqual(Expected, AnnotatedForms).

%% Arity-1 functions taking and returning an argument

typecheck_and_annotate_for_function_taking_an_atom_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(b atom) atom { b }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => b,
                                      type => {type, #{line => 3,
                                                       spec => atom,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          spec => b,
                                          type => {type, #{line => 3,
                                                           source => rufus_text,
                                                           spec => atom}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => atom,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_bool_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(b bool) bool { b }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => b,
                                      type => {type, #{line => 3,
                                                       spec => bool,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          spec => b,
                                          type => {type, #{line => 3,
                                                           source => rufus_text,
                                                           spec => bool}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => bool,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_float_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(n float) float { n }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => n,
                                      type => {type, #{line => 3,
                                                       spec => float,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          spec => n,
                                          type => {type, #{line => 3,
                                                           source => rufus_text,
                                                           spec => float}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => float,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_an_int_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func, #{params => [{param, #{line => 3,
                                      spec => n,
                                      type => {type, #{line => 3,
                                                       spec => int,
                                                       source => rufus_text}}}}],
                 exprs => [{identifier, #{line => 3,
                                          spec => n,
                                          type => {type, #{line => 3,
                                                           source => rufus_text,
                                                           spec => int}}}}],
                 line => 3,
                 return_type => {type, #{line => 3,
                                         spec => int,
                                         source => rufus_text}},
                 spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

typecheck_and_annotate_for_function_taking_a_string_and_returning_it_test() ->
    RufusText = "
    module example
    func Echo(s string) string { s }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{identifier, #{line => 3,
                                                  spec => s,
                                                  type => {type, #{line => 3,
                                                                   source => rufus_text,
                                                                   spec => string}}}}],
                         line => 3,
                         params => [{param, #{line => 3,
                                              spec => s,
                                              type => {type, #{line => 3,
                                                               source => rufus_text,
                                                               spec => string}}}}],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => string}},
                         spec => 'Echo'}}],
    ?assertEqual(Expected, AnnotatedForms).

%% func expressions that have various return value and return type combinations

typecheck_and_annotate_function_with_literal_return_value_test() ->
    RufusText = "
    module example
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual({ok, Forms}, rufus_expr:typecheck_and_annotate(Forms)).

typecheck_and_annotate_function_with_unmatched_return_types_test() ->
    RufusText = "
    module example
    func Number() int { 42.0 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = {error, unmatched_return_type, #{expr => {float_lit, #{line => 3,
                                                                      spec => 42.0,
                                                                      type => {type, #{line => 3,
                                                                                       source => inferred,
                                                                                       spec => float}}}},
                                                return_type => {type, #{line => 3,
                                                                        source => rufus_text,
                                                                        spec => int}}}},
    ?assertEqual(Expected, rufus_expr:typecheck_and_annotate(Forms)).