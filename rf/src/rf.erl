%% rf defines command-line tools for working with Rufus programs.
-module(rf).

%% API exports

-export([main/1]).

%% escript entry point

-spec main(list()) -> no_return().
main(Args) ->
    ok = application:start(rf),
    ExitCode = case Args of
        [Command|CommandArgs] ->
            run(Command, CommandArgs);
        [] ->
            -1
    end,
    ok = application:stop(rf),
    erlang:halt(ExitCode).

%% Private API

-spec run(string(), list(string())) -> integer().
run("compile", _Args) ->
    RufusText ="
    module example

    func Number() int {
        42
    }
",
    io:format("RufusText =>~n    ~s~n", [RufusText]),
    {ok, example} = rufus_compile:eval(RufusText),
    io:format("example:Number() =>~n~n    ~p~n", [example:'Number'()]),
    0;
run("debug:erlang-forms", [Filename]) ->
    {ok, BinaryContents} = file:read_file(Filename),
    io:format("ErlangText =>~n~n~s~n", [binary_to_list(BinaryContents)]),
    Tokens = scan(erl_scan:tokens([], binary_to_list(BinaryContents), 1), []),
    Parse = fun(X) -> {ok,Y} = erl_parse:parse_form(X), Y end,
    Forms = [Parse(X) || X <- Tokens],
    io:format("ErlangForms =>~n~n    ~p~n", [Forms]),
    0;
run("debug:rufus-forms", [Filename]) ->
    {ok, BinaryContents} = file:read_file(Filename),
    io:format("RufusText =>~n~n~s~n", [binary_to_list(BinaryContents)]),

    Tokens = scan(erl_scan:tokens([], binary_to_list(BinaryContents), 1), []),
    Parse = fun(X) -> {ok,Y} = erl_parse:parse_form(X), Y end,
    Forms = [Parse(X) || X <- Tokens],
    io:format("RufusForms =>~n~n    ~p~n", [Forms]),
    0;
run("debug:parse", _Args) ->
    RufusText = "
    module rand

    func Int() int {
        42
    }
",
    io:format("RufusText =>~n    ~s~n", [RufusText]),
    {ok, Tokens, _Lines} = rufus_scan:string(RufusText),
    io:format("Tokens =>~n~n    ~p~n~n", [Tokens]),
    case rufus_parse:parse(Tokens) of
        {ok, Forms} ->
            io:format("Forms =>~n~n    ~p~n", [Forms]),
            0;
        {error, {Line, _, [{ErrorPrefix, ErrorSuffix}, Token]}} ->
            Error = {error, {Line, {ErrorPrefix, ErrorSuffix}, Token}},
            io:format("Error =>~n~n    ~p~n", [Error]),
            -1;
        {error, {Line, _, [Error, Token]}} ->
            Error = {error, {Line, Error, Token}},
            io:format("Error =>~n~n    ~p~n", [Error]),
            -1
    end;
run("debug:scan", _Args) ->
    RufusText = "
    module example

    func Greet(name string) string {
        \"Hello \" + name
    }
",
    io:format("RufusText =>~n    ~s~n", [RufusText]),
    {ok, Tokens, _Lines} = rufus_scan:string(RufusText),
    io:format("Tokens =>~n~n    ~p~n", [Tokens]),
    0;
run("version", _Args) ->
    case application:get_key(rf, vsn) of
        {ok, Version} ->
            io:format("Rufus v~s~n", [Version]),
            0;
        Error ->
            io:format("Error: ~p~n", [Error]),
            -1
    end;
run("help", ["debug:erlang-forms"]) ->
    io:format("Usage:~n"),
    io:format("~n"),
    io:format("    rf debug:erlang-forms FILE~n"),
    io:format("~n"),
    io:format("Read Erlang source code from a file and print it to the screen as Erlang~n"),
    io:format("abstract forms.~n"),
    io:format("~n"),
    0;
run("help", Args) ->
    help(Args);
run(Command, Args) ->
    warn(Command, Args),
    help(Args).

help(_Args) ->
    io:format("rf provides tools for working with Rufus programs.~n"),
    io:format("~n"),
    io:format("Usage:~n"),
    io:format("~n"),
    io:format("    rf COMMAND [OPTION]...~n"),
    io:format("~n"),
    io:format("The commands are:~n"),
    io:format("~n"),
    io:format("    compile             Compile source code and then run it and print its output~n"),
    io:format("    debug:erlang-forms  Print Erlang source code from a file as abstract forms~n"),
    io:format("    debug:parse         Parse source code and print parse forms~n"),
    io:format("    debug:scan          Scan source code and print parse tokens~n"),
    io:format("    version             Print Rufus version~n"),
    io:format("~n"),
    io:format("Use \"rf help [command]\" for more information about that command~n"),
    io:format("~n"),
    io:format("Additional help topics:~n"),
    io:format("~n"),
    io:format("    spec            language specification~n"),
    io:format("~n"),
    io:format("Use \"rf help [topic]\" for more information about that topic~n"),
    io:format("~n"),
    0.

warn(Command, _Args) ->
    io:format("Unknown command: ~s~n", [Command]).

scan({done, {ok, Token, Line}, CharSpec}, Rest) ->
    scan(erl_scan:tokens([], CharSpec, Line), [Token|Rest]);
scan(_, Res) ->
    lists:reverse(Res).
