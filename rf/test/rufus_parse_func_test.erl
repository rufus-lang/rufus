-module(rufus_parse_func_test).

-include_lib("eunit/include/eunit.hrl").

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
        {func, #{params => [],
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
        {func, #{params => [],
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
        {func, #{params => [],
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
        {func, #{params => [],
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
        {func, #{params => [],
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
        {func, #{params => [],
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
        {func, #{params => [],
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
        {func, #{params => [],
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
        {func, #{params => [{param, #{line => 3,
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
        {func, #{params => [{param, #{line => 3,
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
     {func, #{params => [{param, #{line => 3,
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
