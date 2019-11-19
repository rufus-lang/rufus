-module(rufus_parse_match_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_with_a_match_that_binds_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping() atom {
        response = :pong
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response}},
                                             line => 4,
                                             right => {atom_lit, #{line => 4,
                                                                   spec => pong,
                                                                   type => {type, #{line => 4,
                                                                                    source => inferred,
                                                                                    spec => atom}}}}}},
                                   {identifier, #{line => 5,
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => atom}},
                         spec => 'Ping'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_bool_literal_test() ->
    RufusText = "
    module example
    func Truthy() bool {
        response = true
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response}},
                                             line => 4,
                                             right => {bool_lit, #{line => 4,
                                                                   spec => true,
                                                                   type => {type, #{line => 4,
                                                                                    source => inferred,
                                                                                    spec => bool}}}}}},
                                   {identifier, #{line => 5,
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => bool}},
                         spec => 'Truthy'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText = "
    module example
    func FortyTwo() float {
        response = 42.0
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response}},
                                             line => 4,
                                             right => {float_lit, #{line => 4,
                                                                    spec => 42.0,
                                                                    type => {type, #{line => 4,
                                                                                     source => inferred,
                                                                                     spec => float}}}}}},
                                   {identifier, #{line => 5,
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => float}},
                         spec => 'FortyTwo'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_an_int_literal_test() ->
    RufusText = "
    module example
    func FortyTwo() int {
        response = 42
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response}},
                                             line => 4,
                                             right => {int_lit, #{line => 4,
                                                                  spec => 42,
                                                                  type => {type, #{line => 4,
                                                                                   source => inferred,
                                                                                   spec => int}}}}}},
                                   {identifier, #{line => 5,
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => int}},
                         spec => 'FortyTwo'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText = "
    module example
    func Ping() string {
        response = \"pong\"
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => response}},
                                             line => 4,
                                             right => {string_lit, #{line => 4,
                                                                     spec => <<"pong">>,
                                                                     type => {type, #{line => 4,
                                                                                      source => inferred,
                                                                                      spec => string}}}}}},
                                   {identifier, #{line => 5,
                                                  spec => response}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => string}},
                         spec => 'Ping'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_variable_test() ->
    RufusText = "
    module example
    func Echo(n int) int {
        m = n
        m
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),

    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                    spec => m}},
                                             line => 4,
                                             right => {identifier, #{line => 4,
                                                                     spec => n}}}},
                                   {identifier, #{line => 5,
                                                  spec => m}}],
                         line => 3,
                         params => [{param, #{line => 3,
                                              spec => n,
                                              type => {type, #{line => 3,
                                                               source => rufus_text,
                                                               spec => int}}}}],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => int}},
                         spec => 'Echo'}}],
    ?assertEqual(Expected, Forms).
