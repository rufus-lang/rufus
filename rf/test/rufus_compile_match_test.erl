-module(rufus_compile_match_test).

-include_lib("eunit/include/eunit.hrl").

eval_a_function_with_a_match_that_binds_an_atom_literal_test() ->
    RufusText = "
    module example
    func Ping() atom {
        response = :pong
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(pong, example:'Ping'()).

eval_a_function_with_a_match_that_binds_a_bool_literal_test() ->
    RufusText = "
    module example
    func Truthy() bool {
        response = true
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({bool, true}, example:'Truthy'()).

eval_a_function_with_a_match_that_binds_a_float_literal_test() ->
    RufusText = "
    module example
    func Pi() float {
        response = 3.14159265359
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3.14159265359, example:'Pi'()).

eval_a_function_with_a_match_that_binds_an_int_literal_test() ->
    RufusText = "
    module example
    func FortyTwo() int {
        response = 42
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(42, example:'FortyTwo'()).

eval_a_function_with_a_match_that_binds_a_string_literal_test() ->
    RufusText = "
    module example
    func Greeting() string {
        response = \"hello\"
        response
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual({string, <<"hello">>}, example:'Greeting'()).

%% match expressions involving binary_op expressions

typecheck_and_annotate_function_with_a_match_that_has_a_left_binary_op_operand_test() ->
    RufusText = "
    module example
    func Random() int {
        n = 3
        1 + 2 = n
    }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

typecheck_and_annotate_function_with_a_match_that_has_a_right_binary_op_operand_test() ->
    RufusText = "
    module example
    func Random() int { n = 1 + 2 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

typecheck_and_annotate_function_with_a_match_that_has_left_and_right_binary_op_operands_test() ->
    RufusText = "
    module example
    func Random() int { 1 + 2 = 2 + 1 }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(3, example:'Random'()).

%% match expressions involving function calls

typecheck_and_annotate_function_with_a_match_that_has_a_right_call_operand_test() ->
    RufusText = "
    module example
    func Two() int { 2 }
    func Random() int { n = Two() }
    ",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(2, example:'Random'()).

typecheck_and_annotate_function_with_a_match_that_has_a_left_call_operand_test() ->
    RufusText = "
    module example
    func Two() int { 2 }
    func Random() int { Two() = 2 }
    ",
    Data = #{form => {match, #{left => {call, #{args => [],
                                                line => 4,
                                                spec => 'Two',
                                                type => {type, #{line => 3,
                                                                 source => rufus_text,
                                                                 spec => int}}}},
                               line => 4,
                               right => {int_lit, #{line => 4,
                                                    spec => 2,
                                                    type => {type, #{line => 4,
                                                                     source => inferred,
                                                                     spec => int}}}}}},
             globals => #{'Random' => [{func, #{exprs => [{match, #{left => {call, #{args => [],
                                                                                     line => 4,
                                                                                     spec => 'Two'}},
                                                                    line => 4,
                                                                    right => {int_lit, #{line => 4,
                                                                                         spec => 2,
                                                                                         type => {type, #{line => 4,
                                                                                                          source => inferred,
                                                                                                          spec => int}}}}}}],
                                                line => 4,
                                                params => [],
                                                return_type => {type, #{line => 4,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Random'}}],
                          'Two' => [{func, #{exprs => [{int_lit, #{line => 3,
                                                                   spec => 2,
                                                                   type => {type, #{line => 3,
                                                                                    source => inferred,
                                                                                    spec => int}}}}],
                                             line => 3,
                                             params => [],
                                             return_type => {type, #{line => 3,
                                                                     source => rufus_text,
                                                                     spec => int}},
                                             spec => 'Two'}}]},
             locals => #{}},
    ?assertEqual({error, illegal_pattern, Data}, rufus_compile:eval(RufusText)).

typecheck_and_annotate_function_with_a_match_that_has_a_left_binary_op_operand_with_a_call_operand_test() ->
    RufusText = "
    module example
    func Two() int { 2 }
    func Random() int {
        n = 3
        1 + Two() = n
    }
    ",
    Data = #{form => {match, #{left => {binary_op, #{left => {int_lit, #{line => 6,
                                                                         spec => 1,
                                                                         type => {type, #{line => 6,
                                                                                          source => inferred,
                                                                                          spec => int}}}},
                                                     line => 6,
                                                     op => '+',
                                                     right => {call, #{args => [],
                                                                       line => 6,
                                                                       spec => 'Two',
                                                                       type => {type, #{line => 3,
                                                                                        source => rufus_text,
                                                                                        spec => int}}}},
                                                     type => {type, #{line => 6,
                                                                      source => inferred,
                                                                      spec => int}}}},
                               line => 6,
                               right => {identifier, #{line => 6,
                                                       spec => n}}}},
             globals => #{'Random' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 5,
                                                                                           spec => n}},
                                                                    line => 5,
                                                                    right => {int_lit, #{line => 5,
                                                                                         spec => 3,
                                                                                         type =>
                                                                                             {type, #{line => 5,
                                                                                                      source => inferred,
                                                                                                      spec => int}}}}}},
                                                          {match, #{left => {binary_op, #{left => {int_lit, #{line => 6,
                                                                                                              spec => 1,
                                                                                                              type =>
                                                                                                                  {type, #{line => 6,
                                                                                                                           source => inferred,
                                                                                                                           spec => int}}}},
                                                                                          line => 6,
                                                                                          op => '+',
                                                                                          right => {call, #{args => [],
                                                                                                            line => 6,
                                                                                                            spec => 'Two'}}}},
                                                                    line => 6,
                                                                    right => {identifier, #{line => 6,
                                                                                            spec => n}}}}],
                                                line => 4,
                                                params => [],
                                                return_type => {type, #{line => 4,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Random'}}],
                          'Two' => [{func, #{exprs => [{int_lit, #{line => 3,
                                                                   spec => 2,
                                                                   type => {type, #{line => 3,
                                                                                    source => inferred,
                                                                                    spec => int}}}}],
                                             line => 3,
                                             params => [],
                                             return_type => {type, #{line => 3,
                                                                     source => rufus_text,
                                                                     spec => int}},
                                             spec => 'Two'}}]},
             locals => #{n => {type, #{line => 5,
                                       source => inferred,
                                       spec => int}}}},
    ?assertEqual({error, illegal_pattern, Data}, rufus_compile:eval(RufusText)).

typecheck_and_annotate_function_with_a_match_that_has_a_left_and_right_call_operand_test() ->
    RufusText = "
    module example
    func Two() int { 2 }
    func Random() int { Two() = Two() }
    ",
    Data = #{form => {match, #{left => {call, #{args => [],
                                                line => 4,
                                                spec => 'Two',
                                                type => {type, #{line => 3,
                                                                 source => rufus_text,
                                                                 spec => int}}}},
                               line => 4,
                               right => {call, #{args => [],
                                                 line => 4,
                                                 spec => 'Two'}}}},
             globals => #{'Random' => [{func, #{exprs => [{match, #{left => {call, #{args => [],
                                                                                     line => 4,
                                                                                     spec => 'Two'}},
                                                                    line => 4,
                                                                    right => {call, #{args => [],
                                                                                      line => 4,
                                                                                      spec => 'Two'}}}}],
                                                line => 4,
                                                params => [],
                                                return_type => {type, #{line => 4,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Random'}}],
                          'Two' => [{func, #{exprs => [{int_lit, #{line => 3,
                                                                   spec => 2,
                                                                   type => {type, #{line => 3,
                                                                                    source => inferred,
                                                                                    spec => int}}}}],
                                             line => 3,
                                             params => [],
                                             return_type => {type, #{line => 3,
                                                                     source => rufus_text,
                                                                     spec => int}},
                                             spec => 'Two'}}]},
             locals => #{}},
    ?assertEqual({error, illegal_pattern, Data}, rufus_compile:eval(RufusText)).

typecheck_and_annotate_function_with_a_match_that_has_a_right_call_operand_with_a_mismatched_left_type_test() ->
    RufusText = "
    module example
    func Two() int { 2 }
    func Random() int {
        n = \"hello\"
        n = Two()
    }
    ",
    Data = #{globals => #{'Random' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 5,
                                                                                           spec => n}},
                                                                    line => 5,
                                                                    right => {string_lit, #{line => 5,
                                                                                            spec => <<"hello">>,
                                                                                            type => {type, #{line => 5,
                                                                                                             source => inferred,
                                                                                                             spec => string}}}}}},
                                                          {match, #{left => {identifier, #{line => 6,
                                                                                           spec => n}},
                                                                    line => 6,
                                                                    right => {call, #{args => [],
                                                                                      line => 6,
                                                                                      spec => 'Two'}}}}],
                                                line => 4,
                                                params => [],
                                                return_type => {type, #{line => 4,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Random'}}],
                          'Two' => [{func, #{exprs => [{int_lit, #{line => 3,
                                                                   spec => 2,
                                                                   type => {type, #{line => 3,
                                                                                    source => inferred,
                                                                                    spec => int}}}}],
                                             line => 3,
                                             params => [],
                                             return_type => {type, #{line => 3,
                                                                     source => rufus_text,
                                                                     spec => int}},
                                             spec => 'Two'}}]},
             left => {identifier, #{line => 6,
                                    spec => n,
                                    type => {type, #{line => 5,
                                                     source => inferred,
                                                     spec => string}}}},
             locals => #{n => {type, #{line => 5,
                                       source => inferred,
                                       spec => string}}},
             right => {call, #{args => [],
                               line => 6,
                               spec => 'Two',
                               type => {type, #{line => 3,
                                                source => rufus_text,
                                                spec => int}}}}},
    ?assertEqual({error, unmatched_types, Data}, rufus_compile:eval(RufusText)).

typecheck_and_annotate_function_with_a_match_that_has_a_right_call_operand_with_a_mismatched_arg_type_test() ->
    RufusText = "
    module example
    func Echo(n int) int { n }
    func Random() int {
        2 = Echo(:two)
    }
    ",
    Data = #{args => [{atom_lit, #{line => 5,
                                   spec => two,
                                   type => {type, #{line => 5,
                                                    source => inferred,
                                                    spec => atom}}}}],
             funcs => [{func, #{exprs => [{identifier, #{line => 3,
                                                         spec => n}}],
                                line => 3,
                                params => [{param, #{line => 3,
                                                     spec => n,
                                                     type => {type, #{line => 3,
                                                                      source => rufus_text,
                                                                      spec => int}}}}],
                                return_type => {type, #{line => 3,
                                                        source => rufus_text,
                                                        spec => int}},
                                spec => 'Echo'}}]},
    ?assertEqual({error, unmatched_args, Data}, rufus_compile:eval(RufusText)).

%% match expressions with type constraint violations

typecheck_and_annotate_function_with_a_match_that_has_an_unbound_variable_test() ->
    RufusText = "
    module example
    func Broken() int {
        value = 1
        value = unbound
    }
    ",
    Data = #{form => {identifier, #{line => 5,
                                    locals => #{value => {type, #{line => 4,
                                                                  source => inferred,
                                                                  spec => int}}},
                                    spec => unbound}},
             globals => #{'Broken' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                                           spec => value}},
                                                                    line => 4,
                                                                    right => {int_lit, #{line => 4,
                                                                                         spec => 1,
                                                                                         type => {type, #{line => 4,
                                                                                                          source => inferred,
                                                                                                          spec => int}}}}}},
                                                          {match, #{left => {identifier, #{line => 5,
                                                                                           spec => value}},
                                                                    line => 5,
                                                                    right => {identifier, #{line => 5,
                                                                                            spec => unbound}}}}],
                                                line => 3,
                                                params => [],
                                                return_type => {type, #{line => 3,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Broken'}}]},
             locals => #{value => {type, #{line => 4,
                                           source => inferred,
                                           spec => int}}}},
    ?assertEqual({error, unbound_variable, Data}, rufus_compile:eval(RufusText)).

typecheck_and_annotate_function_with_a_match_that_has_unbound_variables_test() ->
    RufusText = "
    module example
    func Broken() int {
        unbound1 = unbound2
    }
    ",
    Data = #{globals => #{'Broken' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                                           spec => unbound1}},
                                                                    line => 4,
                                                                    right => {identifier, #{line => 4,
                                                                                            spec => unbound2}}}}],
                                                line => 3,
                                                params => [],
                                                return_type => {type, #{line => 3,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Broken'}}]},
             left => {identifier, #{line => 4,
                                    locals => #{},
                                    spec => unbound1}},
             locals => #{},
             right => {identifier, #{line => 4,
                                     locals => #{},
                                     spec => unbound2}}},
    ?assertEqual({error, unbound_variables, Data}, rufus_compile:eval(RufusText)).

typecheck_and_annotate_function_with_a_match_that_has_unmatched_types_test() ->
    RufusText = "
    module example
    func Broken() int {
        a = :hello
        i = 42
        a = i
        i
    }
    ",
    Data = #{globals => #{'Broken' => [{func, #{exprs => [{match, #{left => {identifier, #{line => 4,
                                                                                           spec => a}},
                                                                    line => 4,
                                                                    right => {atom_lit, #{line => 4,
                                                                                          spec => hello,
                                                                                          type => {type, #{line => 4,
                                                                                                           source => inferred,
                                                                                                           spec => atom}}}}}},
                                                          {match, #{left => {identifier, #{line => 5,
                                                                                           spec => i}},
                                                                    line => 5,
                                                                    right => {int_lit, #{line => 5,
                                                                                         spec => 42,
                                                                                         type => {type, #{line => 5,
                                                                                                          source => inferred,
                                                                                                          spec => int}}}}}},
                                                          {match, #{left => {identifier, #{line => 6,
                                                                                           spec => a}},
                                                                    line => 6,
                                                                    right => {identifier, #{line => 6,
                                                                                            spec => i}}}},
                                                          {identifier, #{line => 7,
                                                                         spec => i}}],
                                                line => 3,
                                                params => [],
                                                return_type => {type, #{line => 3,
                                                                        source => rufus_text,
                                                                        spec => int}},
                                                spec => 'Broken'}}]},
             left => {identifier, #{line => 6,
                                    spec => a,
                                    type => {type, #{line => 4,
                                                     source => inferred,
                                                     spec => atom}}}},
             locals => #{a => {type, #{line => 4,
                                       source => inferred,
                                       spec => atom}},
                         i => {type, #{line => 5,
                                       source => inferred,
                                       spec => int}}},
             right => {identifier, #{line => 6,
                                     spec => i,
                                     type => {type, #{line => 5,
                                                      source => inferred,
                                                      spec => int}}}}},
    ?assertEqual({error, unmatched_types, Data}, rufus_compile:eval(RufusText)).
