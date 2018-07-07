-module(rf).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args // ~p~n", [Args]),
    String = "A in (22,33,44) and B in ('abc','xyz')",
    Spec = rfc_parser:parse_string(String),
    TestData0 = dict:store('A', 33, dict:new()),
    TestData1 = dict:store('B', "abc", TestData0),
    Result = rfc_parser:matches(Spec, TestData1),
    io:format("rfc_parser:matches(Spec, TestData1) // ~p~n", [Result]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
