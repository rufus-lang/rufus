-module(rufus_type_test).

-include_lib("eunit/include/eunit.hrl").

resolve_unknown_func_error_test() ->
    Form = rufus_form:make_apply('Ping', [], 7),
    Expected = {error, unknown_func, #{args => [],
                                       spec => 'Ping'}},
    ?assertEqual(Expected, rufus_type:resolve(#{}, Form)).

resolve_unknown_arity_error_test() ->
    RufusText = "
    module example
    func Ping(message string) string { message }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Ping', [], 7),
    Data = #{arg_exprs => [],
             form_decls => [{func_decl, #{args => [{arg_decl, #{line => 3,
                                                                spec => message,
                                                                type => {type, #{line => 3,
                                                                                 source => rufus_text,
                                                                                 spec => string}}}}],
                                          exprs => [{identifier, #{line => 3,
                                                                   spec => message}}],
                                          line => 3,
                                          return_type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => string}},
                                          spec => 'Ping'}}]},
    ?assertEqual({error, unknown_arity, Data}, rufus_type:resolve(Globals, Form)).


resolve_form_with_a_mismatched_argument_test() ->
    RufusText = "
    module example
    func Echo(text string) string { text }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Echo', [rufus_form:make_literal(integer, 42, 7)], 7),
    Data = #{arg_exprs => [{integer_lit, #{line => 7,
                                           spec => 42,
                                           type => {type, #{line => 7,
                                                            source => inferred,
                                                            spec => integer}}}}],
             form_decls => [{func_decl, #{args => [{arg_decl, #{line => 3,
                                                                spec => text,
                                                                type => {type, #{line => 3,
                                                                                 source => rufus_text,
                                                                                 spec => string}}}}],
                                          exprs => [{identifier, #{line => 3,
                                                                   spec => text}}],
                                          line => 3,
                                          return_type => {type, #{line => 3,
                                                                  source => rufus_text,
                                                                  spec => string}},
                                          spec => 'Echo'}}]},
    ?assertEqual({error, unmatched_args, Data}, rufus_type:resolve(Globals, Form)).

resolve_form_with_an_existing_type_form_test() ->
    Form = rufus_form:make_literal(bool, true, 7),
    Expected = {type, #{spec => bool, source => inferred, line => 7}},
    ?assertEqual({ok, Expected}, rufus_type:resolve(#{}, Form)).

resolve_form_with_one_argument_test() ->
    RufusText = "
    module example
    func Echo(text string) string { text }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7),
    Expected = {type, #{line => 3, source => rufus_text, spec => string}},
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_form_with_two_argument_test() ->
    RufusText = "
    module example
    func Concatenate(a atom, b string) string { \"hello world\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_scope:globals(Forms),
    Form = rufus_form:make_apply('Concatenate', [rufus_form:make_literal(atom, hello, 7),
                                                 rufus_form:make_literal(string, <<"world">>, 7)], 7),
    Expected = {type, #{line => 3, source => rufus_text, spec => string}},
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).
