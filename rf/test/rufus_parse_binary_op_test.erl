-module(rufus_parse_binary_op_test).

-include_lib("eunit/include/eunit.hrl").

parse_function_adding_two_ints_test() ->
    RufusText = "
    module math
    func Three() int { 1 + 2 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2,
                spec => math}},
     {func, #{params => [],
              exprs => [{binary_op, #{line => 3,
                                      op => '+',
                                      left => {int_lit, #{line => 3,
                                                          spec => 1,
                                                          type => {type, #{line => 3,
                                                                           spec => int,
                                                                           source => inferred}}}},
                                      right => {int_lit, #{line => 3,
                                                           spec => 2,
                                                           type => {type, #{line => 3,
                                                                            spec => int,
                                                                            source => inferred}}}}}}],
              line => 3,
              return_type => {type, #{line => 3,
                                      spec => int,
                                      source => rufus_text}},
              spec => 'Three'}}
    ], Forms).

parse_function_adding_three_ints_test() ->
    RufusText = "
    module math
    func Six() int { 1 + 2 + 3 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func, #{params => [],
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
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2,
                spec => math}},
     {func, #{params => [],
              exprs => [{binary_op, #{line => 3,
                                      op => '-',
                                      left => {int_lit, #{line => 3,
                                                          spec => 2,
                                                          type => {type, #{line => 3,
                                                                           spec => int,
                                                                           source => inferred}}}},
                                      right => {int_lit, #{line => 3,
                                                           spec => 1,
                                                           type => {type, #{line => 3,
                                                                            spec => int,
                                                                            source => inferred}}}}}}],
              line => 3,
              return_type => {type, #{line => 3,
                                      spec => int,
                                      source => rufus_text}},
              spec => 'One'}}
    ], Forms).

parse_function_subtracting_three_ints_test() ->
    RufusText = "
    module math
    func MinusNine() int { 3 - 5 - 7 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func, #{params => [],
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
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2,
                spec => math}},
     {func, #{params => [],
              exprs => [{binary_op, #{line => 3,
                                      op => '*',
                                      left => {int_lit, #{line => 3,
                                                          spec => 2,
                                                          type => {type, #{line => 3,
                                                                           spec => int,
                                                                           source => inferred}}}},
                                      right => {int_lit, #{line => 3,
                                                           spec => 21,
                                                           type => {type, #{line => 3,
                                                                            spec => int,
                                                                            source => inferred}}}}}}],
              line => 3,
              return_type => {type, #{line => 3,
                                      spec => int,
                                      source => rufus_text}},
              spec => 'FortyTwo'}}
    ], Forms).

parse_function_multiplying_three_ints_test() ->
    RufusText = "
    module math
    func OneTwenty() int { 4 * 5 * 6 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func, #{params => [],
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

parse_function_dividing_two_ints_test() ->
    RufusText = "
    module math
    func FortyTwo() int { 84 / 2 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2,
                spec => math}},
     {func, #{params => [],
              exprs => [{binary_op, #{line => 3,
                                      op => '/',
                                      left => {int_lit, #{line => 3,
                                                          spec => 84,
                                                          type => {type, #{line => 3,
                                                                           spec => int,
                                                                           source => inferred}}}},
                                      right => {int_lit, #{line => 3,
                                                           spec => 2,
                                                           type => {type, #{line => 3,
                                                                            spec => int,
                                                                            source => inferred}}}}}}],
              line => 3,
              return_type => {type, #{line => 3,
                                      spec => int,
                                      source => rufus_text}},
              spec => 'FortyTwo'}}
    ], Forms).

parse_function_dividing_three_ints_test() ->
    RufusText = "
    module math
    func Five() int { 100 / 10 / 2 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func, #{params => [],
               exprs => [{binary_op, #{left => {binary_op, #{op => '/',
                                                             left => {int_lit, #{line => 3,
                                                                                 spec => 100,
                                                                                 type => {type, #{line => 3,
                                                                                                  spec => int,
                                                                                                  source => inferred}}}},
                                                             right => {int_lit, #{line => 3,
                                                                                  spec => 10,
                                                                                  type => {type, #{line => 3,
                                                                                                   spec => int,
                                                                                                   source => inferred}}}},
                                                             line => 3}},
                                       line => 3,
                                       op => '/',
                                       right => {int_lit, #{line => 3,
                                                            spec => 2,
                                                            type => {type, #{line => 3,
                                                                             spec => int,
                                                                             source => inferred}}}}}}],
               line => 3,
               return_type => {type, #{line => 3,
                                       spec => int,
                                       source => rufus_text}},
               spec => 'Five'}}
    ], Forms).

parse_function_remaindering_two_ints_test() ->
    RufusText = "
    module math
    func Six() int { 27 % 7 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2,
                spec => math}},
     {func, #{params => [],
              exprs => [{binary_op, #{line => 3,
                                      op => '%',
                                      left => {int_lit, #{line => 3,
                                                          spec => 27,
                                                          type => {type, #{line => 3,
                                                                           spec => int,
                                                                           source => inferred}}}},
                                      right => {int_lit, #{line => 3,
                                                           spec => 7,
                                                           type => {type, #{line => 3,
                                                                            spec => int,
                                                                            source => inferred}}}}}}],
              line => 3,
              return_type => {type, #{line => 3,
                                      spec => int,
                                      source => rufus_text}},
              spec => 'Six'}}
    ], Forms).

parse_function_remaindering_three_ints_test() ->
    RufusText = "
    module math
    func Four() int { 100 % 13 % 5 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func, #{params => [],
               exprs => [{binary_op, #{left => {binary_op, #{op => '%',
                                                             left => {int_lit, #{line => 3,
                                                                                 spec => 100,
                                                                                 type => {type, #{line => 3,
                                                                                                  spec => int,
                                                                                                  source => inferred}}}},
                                                             right => {int_lit, #{line => 3,
                                                                                  spec => 13,
                                                                                  type => {type, #{line => 3,
                                                                                                   spec => int,
                                                                                                   source => inferred}}}},
                                                             line => 3}},
                                       line => 3,
                                       op => '%',
                                       right => {int_lit, #{line => 3,
                                                            spec => 5,
                                                            type => {type, #{line => 3,
                                                                             spec => int,
                                                                             source => inferred}}}}}}],
               line => 3,
               return_type => {type, #{line => 3,
                                       spec => int,
                                       source => rufus_text}},
               spec => 'Four'}}
    ], Forms).

parse_function_anding_two_bools_test() ->
    RufusText = "
    module example
    func Falsy() bool { true and false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{binary_op, #{left => {bool_lit, #{line => 3,
                                                                      spec => true,
                                                                      type => {type, #{line => 3,
                                                                                       source => inferred,
                                                                                       spec => bool}}}},
                                                 line => 3,
                                                 op => 'and',
                                                 right => {bool_lit, #{line => 3,
                                                                       spec => false,
                                                                       type => {type, #{line => 3,
                                                                                        source => inferred,
                                                                                        spec => bool}}}}}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => bool}},
                         spec => 'Falsy'}}],
    ?assertEqual(Expected, Forms).

parse_function_oring_two_bools_test() ->
    RufusText = "
    module example
    func Truthy() bool { true or false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{binary_op, #{left => {bool_lit, #{line => 3,
                                                                      spec => true,
                                                                      type => {type, #{line => 3,
                                                                                       source => inferred,
                                                                                       spec => bool}}}},
                                                 line => 3,
                                                 op => 'or',
                                                 right => {bool_lit, #{line => 3,
                                                                       spec => false,
                                                                       type => {type, #{line => 3,
                                                                                        source => inferred,
                                                                                        spec => bool}}}}}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => bool}},
                         spec => 'Truthy'}}],
    ?assertEqual(Expected, Forms).

parse_function_xoring_two_bools_test() ->
    RufusText = "
    module example
    func Truthy() bool { true xor false }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [{module, #{line => 2,
                           spec => example}},
                {func, #{exprs => [{binary_op, #{left => {bool_lit, #{line => 3,
                                                                      spec => true,
                                                                      type => {type, #{line => 3,
                                                                                       source => inferred,
                                                                                       spec => bool}}}},
                                                 line => 3,
                                                 op => 'xor',
                                                 right => {bool_lit, #{line => 3,
                                                                       spec => false,
                                                                       type => {type, #{line => 3,
                                                                                        source => inferred,
                                                                                        spec => bool}}}}}}],
                         line => 3,
                         params => [],
                         return_type => {type, #{line => 3,
                                                 source => rufus_text,
                                                 spec => bool}},
                         spec => 'Truthy'}}],
    ?assertEqual(Expected, Forms).
