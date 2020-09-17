-module(rufus_parse_match_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_with_a_match_that_binds_an_atom_literal_test() ->
    RufusText = "
    func Ping() atom {
        response = :pong
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{func, #{exprs => [{match, #{left => {identifier, #{line => 3,
                                                                    spec => response}},
                                             line => 3,
                                             right => {atom_lit, #{line => 3,
                                                                   spec => pong,
                                                                   type => {type, #{line => 3,
                                                                                    source => inferred,
                                                                                    spec => atom}}}}}},
                                   {identifier, #{line => 4,
                                                  spec => response}}],
                         line => 2,
                         params => {params, #{line => 2,
                                              params => []}},
                         return_type => {type, #{line => 2,
                                                 source => rufus_text,
                                                 spec => atom}},
                         spec => 'Ping'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_bool_literal_test() ->
    RufusText = "
    func Truthy() bool {
        response = true
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{func, #{exprs => [{match, #{left => {identifier, #{line => 3,
                                                                    spec => response}},
                                             line => 3,
                                             right => {bool_lit, #{line => 3,
                                                                   spec => true,
                                                                   type => {type, #{line => 3,
                                                                                    source => inferred,
                                                                                    spec => bool}}}}}},
                                   {identifier, #{line => 4,
                                                  spec => response}}],
                         line => 2,
                         params => {params, #{line => 2,
                                              params => []}},
                         return_type => {type, #{line => 2,
                                                 source => rufus_text,
                                                 spec => bool}},
                         spec => 'Truthy'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText = "
    func FortyTwo() float {
        response = 42.0
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{func, #{exprs => [{match, #{left => {identifier, #{line => 3,
                                                                    spec => response}},
                                             line => 3,
                                             right => {float_lit, #{line => 3,
                                                                    spec => 42.0,
                                                                    type => {type, #{line => 3,
                                                                                     source => inferred,
                                                                                     spec => float}}}}}},
                                   {identifier, #{line => 4,
                                                  spec => response}}],
                         line => 2,
                         params => {params, #{line => 2,
                                              params => []}},
                         return_type => {type, #{line => 2,
                                                 source => rufus_text,
                                                 spec => float}},
                         spec => 'FortyTwo'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText = "
    func Ping() string {
        response = \"pong\"
        response
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{func, #{exprs => [{match, #{left => {identifier, #{line => 3,
                                                                    spec => response}},
                                             line => 3,
                                             right => {string_lit, #{line => 3,
                                                                     spec => <<"pong">>,
                                                                     type => {type, #{line => 3,
                                                                                      source => inferred,
                                                                                      spec => string}}}}}},
                                   {identifier, #{line => 4,
                                                  spec => response}}],
                         line => 2,
                         params => {params, #{line => 2,
                                              params => []}},
                         return_type => {type, #{line => 2,
                                                 source => rufus_text,
                                                 spec => string}},
                         spec => 'Ping'}}],
    ?assertEqual(Expected, Forms).

parse_function_with_a_match_that_binds_a_variable_test() ->
    RufusText = "
    func Echo(n int) int {
        m = n
        m
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),

    Expected = [{func, #{exprs => [{match, #{left => {identifier, #{line => 3,
                                                                    spec => m}},
                                             line => 3,
                                             right => {identifier, #{line => 3,
                                                                     spec => n}}}},
                                   {identifier, #{line => 4,
                                                  spec => m}}],
                         line => 2,
                         params => {params, #{line => 2,
                                              params => [{param, #{line => 2,
                                                                   spec => n,
                                                                   type => {type, #{line => 2,
                                                                                    source => rufus_text,
                                                                                    spec => int}}}}]}},
                         return_type => {type, #{line => 2,
                                                 source => rufus_text,
                                                 spec => int}},
                         spec => 'Echo'}}],
    ?assertEqual(Expected, Forms).
