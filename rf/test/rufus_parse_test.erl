-module(rufus_parse_test).

-include_lib("eunit/include/eunit.hrl").

%% Modules

parse_empty_module_test() ->
    {ok, Tokens} = rufus_tokenize:string("module empty"),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 1,
                   spec => empty}}
    ], Forms).

%% %% Import

parse_import_test() ->
    RufusText = "
    module foo
    import \"bar\"
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => foo}},
        {import, #{line => 3,
                   spec => "bar"}}
    ], Forms).

%% Arity-0 functions returning a literal value for scalar types

parse_function_returning_an_atom_test() ->
    RufusText = "
    module example
    func Color() atom { :indigo }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [],
                      exprs => [{atom_lit, #{line => 3,
                                             spec => indigo,
                                             type => {type, #{line => 3,
                                                              spec => atom,
                                                              source => inferred}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              spec => atom,
                                              source => rufus_text}},
                      spec => 'Color'}}
    ], Forms).

parse_function_returning_a_bool_test() ->
    RufusText = "
    module example
    func True() bool { true }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [],
                      exprs => [{bool_lit, #{line => 3,
                                             spec => true,
                                             type => {type, #{line => 3,
                                                              spec => bool,
                                                              source => inferred}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              spec => bool,
                                              source => rufus_text}},
                      spec => 'True'}}
    ], Forms).

parse_function_returning_a_float_test() ->
    RufusText = "
    module math
    func Pi() float { 3.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => math}},
        {func_decl, #{args => [],
                      exprs => [{float_lit, #{line => 3,
                                              spec => 3.14159265359,
                                              type => {type, #{line => 3,
                                                               spec => float,
                                                               source => inferred}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              spec => float,
                                              source => rufus_text}},
                      spec => 'Pi'}}
    ], Forms).

parse_function_returning_an_int_test() ->
    RufusText = "
    module rand
    func Number() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => rand}},
        {func_decl, #{args => [],
                      exprs => [{int_lit, #{line => 3,
                                            spec => 42,
                                            type => {type, #{line => 3,
                                                             spec => int,
                                                             source => inferred}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              spec => int,
                                              source => rufus_text}},
                      spec => 'Number'}}
    ], Forms).

parse_function_returning_a_string_test() ->
    RufusText = "
    module example
    func Greeting() string { \"Hello\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [],
                      exprs => [{string_lit, #{line => 3,
                                               spec => <<"Hello">>,
                                               type => {type, #{line => 3,
                                                                spec => string,
                                                                source => inferred}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              spec => string,
                                              source => rufus_text}},
                      spec => 'Greeting'}}
    ], Forms).

%% Arity-0 functions with multiple function expressions

forms_for_function_with_multiple_expressions_test() ->
    RufusText = "
    module example
    func Multiple() atom {
        42
        :fortytwo
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [],
                      exprs => [{int_lit, #{line => 4,
                                            spec => 42,
                                            type => {type, #{line => 4,
                                                             source => inferred,
                                                             spec => int}}}},
                                {atom_lit, #{line => 5,
                                             spec => fortytwo,
                                             type => {type, #{line => 5,
                                                              source => inferred,
                                                              spec => atom}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              source => rufus_text,
                                              spec => atom}},
                      spec => 'Multiple'}}
    ],
    ?assertEqual(Expected, Forms).

forms_for_function_with_multiple_expressions_with_blank_lines_test() ->
    RufusText = "
    module example
    func Multiple() atom {
        42

        :fortytwo
    }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [],
                      exprs => [{int_lit, #{line => 4,
                                            spec => 42,
                                            type => {type, #{line => 4,
                                                             source => inferred,
                                                             spec => int}}}},
                                {atom_lit, #{line => 6,
                                             spec => fortytwo,
                                             type => {type, #{line => 6,
                                                              source => inferred,
                                                              spec => atom}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              source => rufus_text,
                                              spec => atom}},
                      spec => 'Multiple'}}
    ],
    ?assertEqual(Expected, Forms).

forms_for_function_with_multiple_expressions_separated_by_semicolons_test() ->
    RufusText = "
    module example
    func Multiple() atom { 42; :fortytwo }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [],
                      exprs => [{int_lit, #{line => 3,
                                            spec => 42,
                                            type => {type, #{line => 3,
                                                             source => inferred,
                                                             spec => int}}}},
                                {atom_lit, #{line => 3,
                                             spec => fortytwo,
                                             type => {type, #{line => 3,
                                                              source => inferred,
                                                              spec => atom}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              source => rufus_text,
                                              spec => atom}},
                      spec => 'Multiple'}}
    ],
    ?assertEqual(Expected, Forms).

forms_for_function_with_multiple_expressions_without_end_of_expression_separator_test() ->
    RufusText = "
    module example
    func Multiple() atom { 42 :fortytwo }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {error, Reason} = rufus_parse:parse(Tokens),
    ?assertEqual({3, rufus_parse, ["syntax error before: ", ["fortytwo"]]}, Reason).

%% Arity-1 functions using an argument

parse_function_taking_an_atom_and_returning_an_atom_test() ->
    RufusText = "
    module example
    func Color(c atom) atom { :indigo }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [{arg_decl, #{line => 3,
                                            spec => c,
                                            type => {type, #{line => 3,
                                                             spec => atom,
                                                             source => rufus_text}}}}],
                      exprs => [{atom_lit, #{line => 3,
                                             spec => indigo,
                                             type => {type, #{line => 3,
                                                              spec => atom,
                                                              source => inferred}}}}],
                      line => 3,
                      return_type => {type, #{line => 3,
                                              spec => atom,
                                              source => rufus_text}},
                      spec => 'Color'}}
    ], Forms).

parse_function_taking_a_bool_and_returning_a_bool_test() ->
    RufusText = "
    module example
    func Echo(n bool) bool { true }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [{arg_decl, #{line => 3,
                                            spec => n,
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
                      spec => 'Echo'}}
    ], Forms).

parse_function_taking_an_float_and_returning_an_float_test() ->
    RufusText = "
    module example
    func Echo(n float) float { 3.14159265359 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [{arg_decl, #{line => 3,
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
                      spec => 'Echo'}}
    ], Forms).

parse_function_taking_an_int_and_returning_an_int_test() ->
    RufusText = "
    module example
    func Echo(n int) int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
        {module, #{line => 2,
                   spec => example}},
        {func_decl, #{args => [{arg_decl, #{line => 3,
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
                      spec => 'Echo'}}
    ], Forms).

parse_function_taking_an_string_and_returning_an_string_test() ->
    RufusText = "
    module example
    func Echo(n string) string { \"Hello\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
     {module, #{line => 2,
                spec => example}},
     {func_decl, #{args => [{arg_decl, #{line => 3,
                                         spec => n,
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
                   spec => 'Echo'}}
    ], Forms).

%% Binary operations

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
     {func_decl, #{args => [],
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
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func_decl, #{args => [],
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
     {func_decl, #{args => [],
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
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func_decl, #{args => [],
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
     {func_decl, #{args => [],
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
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func_decl, #{args => [],
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
     {func_decl, #{args => [],
                   exprs => [{binary_op, #{line => 3,
                                           op => '/',
                                           left => {int_lit, #{line => 3,
                                                               spec => 84,
                                                               type => {type, #{line => 3, spec => int, source => inferred}}}},
                                           right => {int_lit, #{line => 3,
                                                                spec => 2,
                                                                type => {type, #{line => 3, spec => int, source => inferred}}}}}}],
                   line => 3,
                   return_type => {type, #{line => 3, spec => int, source => rufus_text}},
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
      {func_decl, #{args => [],
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
     {func_decl, #{args => [],
                   exprs => [{binary_op, #{line => 3,
                                           op => '%',
                                           left => {int_lit, #{line => 3,
                                                               spec => 27,
                                                               type => {type, #{line => 3, spec => int, source => inferred}}}},
                                           right => {int_lit, #{line => 3,
                                                                spec => 7,
                                                                type => {type, #{line => 3, spec => int, source => inferred}}}}}}],
                   line => 3,
                   return_type => {type, #{line => 3, spec => int, source => rufus_text}},
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
      {func_decl, #{args => [],
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

%% Arity-0 functions calling an arity-0 function

parse_function_calling_a_function_without_arguments_test() ->
    RufusText = "
    module math
    func Four() int { 100 % 13 % 5 }
    func Random() int { Four() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => math}},
      {func_decl, #{args => [],
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
                    spec => 'Four'}},
      {func_decl, #{args => [],
                    exprs => [{apply, #{args => [], line => 4, spec => 'Four'}}],
                    line => 4,
                    return_type => {type, #{line => 4,
                                            source => rufus_text,
                                            spec => int}},
                    spec => 'Random'}}
    ], Forms).

%% Arity-0 functions calling an arity-1 function

parse_function_calling_a_function_with_an_argument_test() ->
    RufusText = "
    module example
    func Echo(n string) string { n }
    func Echo() string { Echo(\"Hello\") }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    ?assertEqual([
      {module, #{line => 2,
                 spec => example}},
      {func_decl, #{args => [{arg_decl, #{line => 3,
                                          spec => n,
                                          type => {type, #{line => 3,
                                                           source => rufus_text,
                                                           spec => string}}}}],
                    exprs => [{identifier, #{line => 3,
                                             spec => n}}],
                    line => 3,
                    return_type => {type, #{line => 3,
                                            source => rufus_text,
                                            spec => string}},
                    spec => 'Echo'}},
      {func_decl, #{args => [],
                    exprs => [{apply, #{args => [{string_lit, #{line => 4,
                                                                spec => <<"Hello">>,
                                                                type => {type, #{line => 4,
                                                                                 source => inferred,
                                                                                 spec => string}}}}],
                                        line => 4,
                                        spec => 'Echo'}}],
                    line => 4,
                    return_type => {type, #{line => 4,
                                            source => rufus_text,
                                            spec => string}},
                    spec => 'Echo'}}
    ], Forms).
