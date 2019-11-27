-module(rufus_type_form_test).

-include_lib("eunit/include/eunit.hrl").

resolve_form_with_no_arguments_test() ->
    RufusText = "
    module example
    func Random() int { 42 }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Random', [], 7),
    Expected = rufus_form:make_type(int, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_form_with_one_argument_test() ->
    RufusText = "
    module example
    func Echo(text string) string { text }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7),
    Expected = rufus_form:make_type(string, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_form_with_one_argument_and_many_function_heads_test() ->
    RufusText = "
    module example
    func Echo(name atom) atom { name }
    func Echo(b bool) bool { b }
    func Echo(n float) float { n }
    func Echo(n int) int { n }
    func Echo(text string) string { text }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Echo', [rufus_form:make_literal(string, <<"hello">>, 7)], 7),
    Expected = rufus_form:make_type(string, 7),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).

resolve_form_with_two_argument_test() ->
    RufusText = "
    module example
    func Concatenate(a atom, b string) string { \"hello world\" }
    ",
    {ok, Tokens} = rufus_tokenize:string(RufusText),
    {ok, Forms} = rufus_parse:parse(Tokens),
    {ok, Globals} = rufus_form:globals(Forms),
    Form = rufus_form:make_call('Concatenate', [rufus_form:make_literal(atom, hello, 7),
                                                rufus_form:make_literal(string, <<"world">>, 7)], 7),
    Expected = rufus_form:make_type(string, 3),
    ?assertEqual({ok, Expected}, rufus_type:resolve(Globals, Form)).
