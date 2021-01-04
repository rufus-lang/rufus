-module(rufus_expr_throw_test).

-include_lib("eunit/include/eunit.hrl").

%% typecheck_and_annotate tests

typecheck_and_annotate_function_with_throw_atom_expression_test() ->
    RufusText =
        "module example\n"
        "func Explode() atom {\n"
        "    throw :kaboom\n"
        "}\n",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, AnnotatedForms} = rufus_expr:typecheck_and_annotate(Forms),
    Expected = [
        {module, #{line => 1, spec => example}},
        {func, #{
            exprs => [
                {throw, #{
                    expr =>
                        {atom_lit, #{
                            line => 3,
                            spec => kaboom,
                            type => {type, #{line => 3, spec => atom}}
                        }},
                    line => 3,
                    type =>
                        {type, #{
                            kind => throw,
                            line => 3,
                            spec => 'throw atom'
                        }}
                }}
            ],
            line => 2,
            params => [],
            return_type => {type, #{line => 2, spec => atom}},
            spec => 'Explode',
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

%% Failure mode tests
