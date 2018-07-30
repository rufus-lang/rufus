-module(rf).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args => ~p~n", [Args]),
    SourceText = "
package example

func Greet(name string) string {
    \"Hello \" + name
}
",
    Tokens = rfc_scan:string(SourceText),
    io:format("~s~n~n", [SourceText]),
    io:format("rfc_leex:string(SourceText) => ~p~n", [Tokens]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
