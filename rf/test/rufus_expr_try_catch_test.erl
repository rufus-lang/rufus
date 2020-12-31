-module(rufus_expr_try_catch_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_with_try_and_catch_blocks_both_returning_atom_literals_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch :error {\n"
        "        :error\n"
        "    }\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {try_catch_after, #{
                    after_exprs => [],
                    catch_clauses => [
                        {catch_clause, #{
                            exprs => [
                                {atom_lit, #{
                                    line => 6,
                                    spec => error,
                                    type =>
                                        {type, #{line => 6, spec => atom}}
                                }}
                            ],
                            line => 5,
                            match_expr =>
                                {atom_lit, #{
                                    line => 5,
                                    spec => error,
                                    type =>
                                        {type, #{line => 5, spec => atom}}
                                }}
                        }}
                    ],
                    line => 3,
                    try_exprs => [
                        {atom_lit, #{
                            line => 4,
                            spec => ok,
                            type => {type, #{line => 4, spec => atom}}
                        }}
                    ],
                    type => {type, #{line => 4, spec => atom}}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Maybe',
            type =>
                {type, #{
                    kind => func,
                    line => 2,
                    param_types => [],
                    return_type => {type, #{line => 2, spec => atom}},
                    spec => 'func() atom'
                }}
        }}
    ],
    ?assertEqual(Expected, AnnotatedForms).
