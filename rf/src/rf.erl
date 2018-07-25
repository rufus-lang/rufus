-module(rf).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args => ~p~n", [Args]),
    Tokens = rfc_leex:string("package math"),
    io:format("rfc_leex:string(String) => ~p~n", [Tokens]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
