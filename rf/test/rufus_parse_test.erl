-module(rufus_parse_test).

-include_lib("eunit/include/eunit.hrl").

%% Modules

parse_empty_module_test() ->
    {ok, Tokens, _} = rufus_scan:string("module empty"),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 1, spec => empty}}
    ], Forms).

%% Import

parse_import_test() ->
    RufusText = "
    module foo
    import \"bar\"
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => foo}},
     {import, #{line => 3, spec => "bar"}}
    ], Forms).

%% Arity-0 functions returning a literal value for scalar types

parse_function_returning_a_bool_test() ->
    RufusText = "
    module example
    func True() bool { true }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => example}},
     {func, #{args => [],
              exprs => [{bool_lit, #{line => 3,
                                     spec => true,
                                     type => {type, #{line => 3, spec => bool, source => inferred}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => bool, source => rufus_text}},
              spec => 'True'}}
    ], Forms).

parse_function_returning_a_float_test() ->
    RufusText = "
    module math
    func Pi() float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => math}},
     {func, #{args => [],
              exprs => [{float_lit, #{line => 3,
                                      spec => 3.14159265359,
                                      type => {type, #{line => 3, spec => float, source => inferred}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => float, source => rufus_text}},
              spec => 'Pi'}}
    ], Forms).

parse_function_returning_an_int_test() ->
    RufusText = "
    module rand
    func Number() int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => rand}},
     {func, #{args => [],
              exprs => [{int_lit, #{line => 3,
                                    spec => 42,
                                    type => {type, #{line => 3, spec => int, source => inferred}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => int, source => rufus_text}},
              spec => 'Number'}}
    ], Forms).

parse_function_returning_a_string_test() ->
    RufusText = "
    module example
    func Greeting() string { \"Hello\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => example}},
     {func, #{args => [],
              exprs => [{string_lit, #{line => 3,
                                       spec => <<"Hello">>,
                                       type => {type, #{line => 3, spec => string, source => inferred}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => string, source => rufus_text}},
              spec => 'Greeting'}}
    ], Forms).

%% Arity-1 functions using an argument

parse_function_taking_a_bool_and_returning_a_bool_test() ->
    RufusText = "
    module example
    func Echo(n bool) bool { true }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => example}},
     {func, #{args => [{arg, #{line => 3,
                               spec => n,
                               type => {type, #{line => 3, spec => bool, source => rufus_text}}}}],
              exprs => [{bool_lit, #{line => 3,
                                     spec => true,
                                     type => {type, #{line => 3, spec => bool, source => inferred}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => bool, source => rufus_text}},
              spec => 'Echo'}}
    ], Forms).

parse_function_taking_an_float_and_returning_an_float_test() ->
    RufusText = "
    module example
    func Echo(n float) float { 3.14159265359 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => example}},
     {func, #{args => [{arg, #{line => 3,
                               spec => n,
                               type => {type, #{line => 3, spec => float, source => rufus_text}}}}],
              exprs => [{float_lit, #{line => 3,
                                      spec => 3.14159265359,
                                      type => {type, #{line => 3, spec => float, source => inferred}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => float, source => rufus_text}},
              spec => 'Echo'}}
    ], Forms).

parse_function_taking_an_int_and_returning_an_int_test() ->
    RufusText = "
    module example
    func Echo(n int) int { 42 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => example}},
     {func, #{args => [{arg, #{line => 3,
                               spec => n,
                               type => {type, #{line => 3, spec => int, source => rufus_text}}}}],
              exprs => [{int_lit, #{line => 3,
                                    spec => 42,
                                    type => {type, #{line => 3, spec => int, source => inferred}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => int, source => rufus_text}},
              spec => 'Echo'}}
    ], Forms).

parse_function_taking_an_string_and_returning_an_string_test() ->
    RufusText = "
    module example
    func Echo(n string) string { \"Hello\" }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module,#{line => 2, spec => example}},
     {func, #{args => [{arg, #{line => 3,
                               spec => n,
                               type => {type, #{line => 3, spec => string, source => rufus_text}}}}],
              exprs => [{string_lit, #{line => 3,
                                       spec => <<"Hello">>,
                                       type => {type, #{line => 3, spec => string, source => inferred}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => string, source => rufus_text}},
              spec => 'Echo'}}
    ], Forms).

%% Binary operations

parse_function_adding_two_ints_test() ->
    RufusText = "
    module math
    func Three() int { 1 + 2 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => math}},
     {func, #{args => [],
              exprs => [{binary_op, #{line => 3,
                                      op => '+',
                                      left => {int_lit, #{line => 3,
                                                          spec => 1,
                                                          type => {type, #{line => 3, spec => int, source => inferred}}}},
                                      right => {int_lit, #{line => 3,
                                                           spec => 2,
                                                           type => {type, #{line => 3, spec => int, source => inferred}}}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => int, source => rufus_text}},
              spec => 'Three'}}
    ], Forms).

parse_function_adding_three_ints_test() ->
    RufusText = "
    module math
    func Six() int { 1 + 2 + 3 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module,#{line => 2,spec => math}},
      {func, #{args => [],
               exprs => [{binary_op, #{left => {binary_op, #{op => '+',
                                                             left => {int_lit, #{line => 3,
                                                                                 spec => 1,
                                                                                 type => {type, #{line => 3,
                                                                                                  spec => int,
                                                                                                  source => inferred}}}},
                                                             right => {int_lit, #{line => 3,
                                                                                  spec => 2,
                                                                                  type => {type, #{line => 3,
                                                                                                   spec => int,
                                                                                                   source => inferred}}}},
                                                             line => 3}},
                                       line => 3,
                                       op => '+',
                                       right => {int_lit, #{line => 3,
                                                            spec => 3,
                                                            type => {type, #{line => 3,
                                                                             spec => int,
                                                                             source => inferred}}}}}}],
               line => 3,
               return_type => {type, #{line => 3,
                                       spec => int,
                                       source => rufus_text}},
               spec => 'Six'}}
    ], Forms).

parse_function_subtracting_two_ints_test() ->
    RufusText = "
    module math
    func One() int { 2 - 1 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => math}},
     {func, #{args => [],
              exprs => [{binary_op, #{line => 3,
                                      op => '-',
                                      left => {int_lit, #{line => 3,
                                                          spec => 2,
                                                          type => {type, #{line => 3, spec => int, source => inferred}}}},
                                      right => {int_lit, #{line => 3,
                                                           spec => 1,
                                                           type => {type, #{line => 3, spec => int, source => inferred}}}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => int, source => rufus_text}},
              spec => 'One'}}
    ], Forms).

parse_function_subtracting_three_ints_test() ->
    RufusText = "
    module math
    func MinusNine() int { 3 - 5 - 7 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module,#{line => 2,spec => math}},
      {func, #{args => [],
               exprs => [{binary_op, #{left => {binary_op, #{op => '-',
                                                             left => {int_lit, #{line => 3,
                                                                                 spec => 3,
                                                                                 type => {type, #{line => 3,
                                                                                                  spec => int,
                                                                                                  source => inferred}}}},
                                                             right => {int_lit, #{line => 3,
                                                                                  spec => 5,
                                                                                  type => {type, #{line => 3,
                                                                                                   spec => int,
                                                                                                   source => inferred}}}},
                                                             line => 3}},
                                       line => 3,
                                       op => '-',
                                       right => {int_lit, #{line => 3,
                                                            spec => 7,
                                                            type => {type, #{line => 3,
                                                                             spec => int,
                                                                             source => inferred}}}}}}],
               line => 3,
               return_type => {type, #{line => 3,
                                       spec => int,
                                       source => rufus_text}},
               spec => 'MinusNine'}}
    ], Forms).

parse_function_multiplying_two_ints_test() ->
    RufusText = "
    module math
    func FortyTwo() int { 2 * 21 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2, spec => math}},
     {func, #{args => [],
              exprs => [{binary_op, #{line => 3,
                                      op => '*',
                                      left => {int_lit, #{line => 3,
                                                          spec => 2,
                                                          type => {type, #{line => 3, spec => int, source => inferred}}}},
                                      right => {int_lit, #{line => 3,
                                                           spec => 21,
                                                           type => {type, #{line => 3, spec => int, source => inferred}}}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => int, source => rufus_text}},
              spec => 'FortyTwo'}}
    ], Forms).

parse_function_multiplying_three_ints_test() ->
    RufusText = "
    module math
    func OneTwenty() int { 4 * 5 * 6 }
    ",
    {ok, Tokens, _} = rufus_scan:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module,#{line => 2,spec => math}},
      {func, #{args => [],
               exprs => [{binary_op, #{left => {binary_op, #{op => '*',
                                                             left => {int_lit, #{line => 3,
                                                                                 spec => 4,
                                                                                 type => {type, #{line => 3,
                                                                                                  spec => int,
                                                                                                  source => inferred}}}},
                                                             right => {int_lit, #{line => 3,
                                                                                  spec => 5,
                                                                                  type => {type, #{line => 3,
                                                                                                   spec => int,
                                                                                                   source => inferred}}}},
                                                             line => 3}},
                                       line => 3,
                                       op => '*',
                                       right => {int_lit, #{line => 3,
                                                            spec => 6,
                                                            type => {type, #{line => 3,
                                                                             spec => int,
                                                                             source => inferred}}}}}}],
               line => 3,
               return_type => {type, #{line => 3,
                                       spec => int,
                                       source => rufus_text}},
               spec => 'OneTwenty'}}
    ], Forms).
