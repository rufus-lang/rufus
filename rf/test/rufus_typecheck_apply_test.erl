-module(rufus_typecheck_apply_test).

-include_lib("eunit/include/eunit.hrl").

%% Arity-0 functions calling an arity-0 function

forms_with_function_calling_a_function_without_arguments_test() ->
    RufusText = "
    module math
    func Four() int { 100 % 13 % 5 }
    func Random() int { Four() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Forms} = rufus_typecheck_apply:forms(Forms).

forms_with_function_calling_a_function_with_a_missing_argument_test() ->
    RufusText = "
    module example
    func Echo(n string) string { \"Hello\" }
    func Broken() string { Echo() }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    Result = rufus_typecheck_apply:forms(Forms),
    ?assertEqual({error, incorrect_arg_count, #{expected => 1, actual => 0}}, Result).

%% forms_with_function_calling_a_function_with_a_mismatched_argument_type_test() ->
%%     RufusText = "
%%     module example
%%     func Echo(n string) string { \"Hello\" }
%%     func Broken() string { Echo(42) }
%%     ",
%%     {ok, Tokens} = rufus_tokenize:string(RufusText),
%%     {ok, Forms} = rufus_parse:parse(Tokens),
%%     Result = rufus_typecheck_apply:forms(Forms),
%%     ?assertEqual({error, invalid_arg_type, #{}}, Result).
