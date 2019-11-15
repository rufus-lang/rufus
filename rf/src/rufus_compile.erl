%% rufus_compile compiles and loads Rufus source code.
-module(rufus_compile).

-include_lib("rufus_type.hrl").

%% API exports

-export([eval/1]).

-ifdef(EUNIT).
-export([compile/1]).
-export([eval_chain/2]).
-endif.

%% API

%% eval tokenizes, parses, typechecks, compiles and loads Rufus source code.
%% Return values:
%% - `{ok, Module}` if compilation completed and `Module` is loaded.
%% - `error` or `{error, ...}` if an error occurs.
-spec eval(rufus_text()) -> ok_tuple() | error_tuple().
eval(RufusText) ->
    CompilationStages = [
        fun rufus_tokenize:string/1,
        fun rufus_parse:parse/1,
        fun rufus_expr:typecheck_and_annotate/1,
        fun rufus_erlang:forms/1,
        fun compile/1
    ],
    eval_chain(RufusText, CompilationStages).

%% Private API

%% eval_chain runs each compilation stage H as H(Input), in order. Each
%% compilation stage must return {ok, Output} on success. Any other response is
%% treated as an error. The output from one compilation stage is provided as
%% input to the next. Processing stops when a compilation stage returns an
%% error.
-spec eval_chain(any(), list(fun((_) -> any()))) -> ok_tuple() | error_tuple() | error_triple().
eval_chain(Input, [H|T]) ->
    case H(Input) of
        {ok, Forms} ->
            eval_chain(Forms, T);
        Error ->
            Error
    end;
eval_chain(Input, []) ->
    {ok, Input}.

%% compile uses the Erlang compiler to compile Erlang abstract forms and loads
%% the resulting module.
-spec compile(list()) -> ok_tuple() | error_tuple().
compile(ErlangForms) ->
    case compile:forms(ErlangForms) of
        {ok, Module, BinaryOrCode, _Warnings} ->
            load(Module, BinaryOrCode);
        {ok, Module, BinaryOrCode} ->
            load(Module, BinaryOrCode);
        {error, Reason} ->
            {error, Reason};
        error ->
            {error, unknown}
    end.

%% load creates or overwrites a module with a code binary.
-spec load(atom(), binary()) -> {ok, atom()} | {error, badarg | badfile | nofile | not_purged | on_load_failure | sticky_directory}.
load(Module, BinaryOrCode) ->
    case code:load_binary(Module, "nofile", BinaryOrCode) of
        {module, Module} ->
            {ok, Module};
        {error, Reason} ->
            {error, Reason}
    end.
