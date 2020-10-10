-module(rufus_parse_call_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions calling an arity-0 function

parse_function_calling_a_function_without_arguments_test() ->
    RufusText = "func Random() int { Four() }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {call, #{
                    args => [],
                    line => 1,
                    spec => 'Four'
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => int
                }},
            spec => 'Random'
        }}
    ],
    ?assertEqual(Expected, Forms).

%% Arity-0 functions calling an arity-1 function

parse_function_calling_a_function_with_an_argument_test() ->
    RufusText = "func Echo() string { Echo(\"Hello\") }",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Expected = [
        {func, #{
            params => [],
            exprs => [
                {call, #{
                    args => [
                        {string_lit, #{
                            line => 1,
                            spec => <<"Hello">>,
                            type =>
                                {type, #{
                                    line => 1,
                                    spec => string
                                }}
                        }}
                    ],
                    line => 1,
                    spec => 'Echo'
                }}
            ],
            line => 1,
            return_type =>
                {type, #{
                    line => 1,
                    spec => string
                }},
            spec => 'Echo'
        }}
    ],
    ?assertEqual(Expected, Forms).
