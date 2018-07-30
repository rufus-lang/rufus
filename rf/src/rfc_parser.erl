-module(rfc_parser).

-export([
    matches/2,
    parse_string/1
]).

%%====================================================================
%% Public functions
%%====================================================================

parse_string(String) ->
    {ok, Tokens, _EndLine} = rfc_leex:string(String),
    {ok, Spec} = rfc_yecc:parse(Tokens),
    Spec.

matches({predicate, {var, Var}, Comparator, Critereon}, Props) ->
    case dict:is_key(Var, Props) of
        false ->
            false;
        true ->
            Value = dict:fetch(Var, Props),
            compare(Comparator, Value, Critereon)
    end;
matches({intersection, P1, P2}, Props) ->
    matches(P1, Props) andalso matches(P2, Props);
matches({union, P1, P2}, Props) ->
    matches(P1, Props) or matches(P2, Props);
matches(_,_) ->
    false.

%%====================================================================
%% Private functions
%%====================================================================

compare(memberof, Value, {list, List}) -> lists:member(Value, List);
compare('=', Value, Critereon) -> Value == Critereon;
compare('>', Value, Critereon) -> Value > Critereon;
compare('<', Value, Critereon) -> Value < Critereon;
compare('>=', Value, Critereon) -> Value >= Critereon;
compare('=<', Value, Critereon) -> Value =< Critereon;
compare(_,_,_) -> false.
