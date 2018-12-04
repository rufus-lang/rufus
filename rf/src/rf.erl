%% rf defines command-line tools for working with Rufus programs.
-module(rf).

%% API exports
-export([main/1]).

%% escript entry point.
main(Args) ->
    case Args of
        ["compile:parse"|CmdArgs] ->
            parse(CmdArgs);
        ["compile:scan"|CmdArgs] ->
            scan(CmdArgs);
        ["version"|CmdArgs] ->
            version(CmdArgs);
        _ ->
            help(Args)
    end,
    erlang:halt(0).

%%====================================================================
%% Commands
%%====================================================================

help(_Args) ->
    io:format("rf provides tools for working with Rufus programs.~n"),
    io:format("~n"),
    io:format("Usage:~n"),
    io:format("~n"),
    io:format("    rf COMMAND [OPTION]...~n"),
    io:format("~n"),
    io:format("The commands are:~n"),
    io:format("~n"),
    io:format("    compile:parse   parse source code and print AST~n"),
    io:format("    compile:scan    scan source code and print tokens~n"),
    io:format("    version         print Rufus version~n"),
    io:format("~n"),
    io:format("Use \"rf help [command]\" for more information about that command~n"),
    io:format("~n"),
    io:format("Additional help topics:~n"),
    io:format("~n"),
    io:format("    spec            language specification~n"),
    io:format("~n"),
    io:format("Use \"rf help [topic]\" for more information about that topic~n"),
    io:format("~n"),
    ok.

version(_Args) ->
    io:format("rufus version v0.1.0~n"),
    ok.

parse(_Args) ->
    SourceText = "
    package rand

    func Int() int {
        42
    }
",
    {ok, Tokens, _Lines} = rfc_scan:string(SourceText),
    {ok, AST} = rfc_parse:parse(Tokens),
    io:format("source text =>~n~s~n~n", [SourceText]),
    io:format("scanned tokens =>~n~n    ~p~n", [AST]),
    ok.

scan(_Args) ->
    SourceText = "
    package example

    func Greet(name string) string {
        \"Hello \" + name
    }
",
    {ok, Tokens, _Lines} = rfc_scan:string(SourceText),
    io:format("source text =>~n~s~n~n", [SourceText]),
    io:format("scanned tokens =>~n~n    ~p~n", [Tokens]),
    ok.
