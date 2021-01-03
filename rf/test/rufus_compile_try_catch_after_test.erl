-module(rufus_compile_try_catch_after_test).

-include_lib("eunit/include/eunit.hrl").

eval_function_with_try_and_catch_blocks_both_returning_an_atom_literal_test() ->
    RufusText =
        "module example\n"
        "func Maybe() atom {\n"
        "    try {\n"
        "        :ok\n"
        "    } catch {\n"
        "        :error\n"
        "    }\n"
        "}\n",
    {ok, example} = rufus_compile:eval(RufusText),
    ?assertEqual(ok, example:'Maybe'()).
