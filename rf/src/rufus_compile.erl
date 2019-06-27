%% rufus_compile compiles and loads Rufus source code.
-module(rufus_compile).

-include_lib("rufus_types.hrl").

%% API exports

-export([eval/1]).

-ifdef(EUNIT).
-export([eval_chain/2]).
-endif.

%% API

%% eval parses, type checks, compiles and loads Rufus source code. Return
%% values:
%% - `{ok, Module}` if compilation completed and `Module` is loaded.
%% - `error` or `{error, ...}` if an error occurs.
-spec eval(rufus_text()) -> ok_tuple() | error_tuple().
eval(RufusText) ->
    Handlers = [fun scan/1,
                fun rufus_parse:parse/1,
                fun rufus_annotate_locals:forms/1,
                fun rufus_typecheck_binary_op:forms/1,
                fun rufus_typecheck_func_return_type:forms/1,
                fun rufus_compile_erlang:forms/1,
                fun compile/1
               ],
    eval_chain(RufusText, Handlers).

%% Private API

%% eval_chain runs each handler as H(Input) in order. Each handler result must
%% match {ok, Output}. Any other response is treated as an error. Processing
%% stops when a handler returns an error.
-spec eval_chain(any(), list(fun((_) -> any()))) -> ok_tuple() | error_tuple().
eval_chain(Input, [H|T]) ->
    case H(Input) of
        {ok, Forms} ->
            eval_chain(Forms, T);
        Error ->
            Error
    end;
eval_chain(Input, []) ->
    {ok, Input}.

-spec scan(rufus_text()) -> ok_tuple() | error_tuple().
scan(RufusText) ->
    case rufus_scan:string(RufusText) of
        {ok, Tokens, _Lines} ->
            {ok, Tokens};
        Error ->
            Error
    end.

-spec compile(list()) -> ok_tuple() | error_tuple().
compile(ErlangForms) ->
    case compile:forms(ErlangForms) of
        {ok, Module, BinaryOrCode, _Warnings} ->
            load(Module, BinaryOrCode);
        {ok, Module, BinaryOrCode} ->
            load(Module, BinaryOrCode);
        {error, Reason} ->
            {error, Reason}
    end.

-spec load(atom(), binary() | term()) -> ok_tuple() | error_tuple().
load(Module, BinaryOrCode) ->
    case code:load_binary(Module, "nofile", BinaryOrCode) of
        {module, Module} ->
            {ok, Module};
        {error, Reason} ->
            {error, Reason}
    end.
