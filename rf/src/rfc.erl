-module(rfc).

%% API exports
-export([
    matches/2,
    parse_string/1
]).

%%====================================================================
%% API functions
%%====================================================================

parse_string(String) ->
    {ok, Tokens, _EndLine} = rfc_grammar:string(String),
    {ok, Spec} = rfc_parser:parse(Tokens),
    Spec.

matches({predicate,{var,Var},Comparator,Critereon}, Props) ->
    case dict:is_key(Var,Props) of
        false ->
            false;
        true ->
            Value = dict:fetch(Var,Props),
            compare(Comparator, Value, Critereon)
    end;
matches({intersection,P1,P2}, Props) ->
    matches(P1,Props) andalso matches(P2,Props);
matches({union,P1,P2}, Props) ->
    matches(P1,Props) or matches(P2,Props);
matches(_,_) ->
    false.

%%====================================================================
%% Internal functions
%%====================================================================

compare(memberof, Value,{list,List}) -> lists:member(Value,List);
compare('=', Value,Critereon) -> Value == Critereon;
compare('>', Value,Critereon) -> Value > Critereon;
compare('<', Value,Critereon) -> Value < Critereon;
compare('>=', Value,Critereon) -> Value >= Critereon;
compare('=<', Value,Critereon) -> Value =< Critereon;
compare(_,_,_) -> false.
