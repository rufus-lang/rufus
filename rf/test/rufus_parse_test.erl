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

%% Arity-0 functions returning a literal value for primitive types

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
                                     type => {type, #{line => 3, spec => bool}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => bool}},
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
                                      type => {type, #{line => 3, spec => float}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => float}},
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
                                    type => {type, #{line => 3, spec => int}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => int}},
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
                                       type => {type, #{line => 3, spec => string}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => string}},
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
                               type => {type, #{line => 3, spec => bool}}}}],
              exprs => [{bool_lit, #{line => 3,
                                     spec => true,
                                     type => {type, #{line => 3, spec => bool}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => bool}},
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
                               type => {type, #{line => 3, spec => float}}}}],
              exprs => [{float_lit, #{line => 3,
                                      spec => 3.14159265359,
                                      type => {type, #{line => 3, spec => float}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => float}},
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
                               type => {type, #{line => 3, spec => int}}}}],
              exprs => [{int_lit, #{line => 3,
                                    spec => 42,
                                    type => {type, #{line => 3, spec => int}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => int}},
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
                               type => {type, #{line => 3, spec => string}}}}],
              exprs => [{string_lit, #{line => 3,
                                       spec => <<"Hello">>,
                                       type => {type, #{line => 3, spec => string}}}}],
              line => 3,
              return_type => {type, #{line => 3, spec => string}},
              spec => 'Echo'}}
    ], Forms).
