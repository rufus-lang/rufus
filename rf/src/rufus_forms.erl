-module(rufus_forms).

-include_lib("rufus_type.hrl").

-export([
    each/2,
    globals/1,
    map/2
]).

%% Enumeration API

%% each invokes Fun with each form in Forms. It always returns ok.
-spec each(rufus_forms(), fun((rufus_form()) -> any())) -> ok | no_return().
each([Form = {binary_op, #{left := Left, right := Right}} | T], Fun) ->
    Fun(Left),
    Fun(Right),
    Fun(Form),
    each(T, Fun);
each([Form = {call, #{args := Args}} | T], Fun) ->
    each(Args, Fun),
    Fun(Form),
    each(T, Fun);
each([Form = {cons, #{head := Head, tail := Tail}} | T], Fun) ->
    Fun(Head),
    case Tail of
        Tail when is_list(Tail) ->
            each(Tail, Fun);
        Tail ->
            Fun(Tail)
    end,
    Fun(Form),
    each(T, Fun);
each([Form = {func, #{params := Params, exprs := Exprs}} | T], Fun) ->
    each(Params, Fun),
    each(Exprs, Fun),
    Fun(Form),
    each(T, Fun);
each([Form = {list_lit, #{elements := Elements}} | T], Fun) ->
    each(Elements, Fun),
    Fun(Form),
    each(T, Fun);
each([Form = {match, #{left := Left, right := Right}} | T], Fun) ->
    Fun(Left),
    Fun(Right),
    Fun(Form),
    each(T, Fun);
each([Form | T], Fun) ->
    Fun(Form),
    each(T, Fun);
each([], _Fun) ->
    ok.

%% map applies Fun to each form in Forms to build and return a new tree.
-spec map(rufus_forms(), fun((rufus_form()) -> rufus_form())) -> rufus_forms().
map(Forms, Fun) ->
    map([], Forms, Fun).

-spec map(rufus_forms(), rufus_forms(), fun((rufus_form()) -> rufus_form())) -> list(rufus_form()).
map(Acc, [{binary_op, Context = #{left := Left, right := Right}} | T], Fun) ->
    AnnotatedLeft = Fun(Left),
    AnnotatedRight = Fun(Right),
    AnnotatedForm = Fun({binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight}}),
    map([AnnotatedForm | Acc], T, Fun);
map(Acc, [{call, Context = #{args := Args}} | T], Fun) ->
    AnnotatedArgs = map(Args, Fun),
    AnnotatedForm = Fun({call, Context#{args => AnnotatedArgs}}),
    map([AnnotatedForm | Acc], T, Fun);
map(Acc, [{cons, Context = #{head := Head, tail := Tail}} | T], Fun) ->
    AnnotatedHead = Fun(Head),
    AnnotatedTail =
        case Tail of
            Tail when is_list(Tail) ->
                map(Tail, Fun);
            Tail ->
                Fun(Tail)
        end,
    AnnotatedForm = Fun({cons, Context#{head => AnnotatedHead, tail => AnnotatedTail}}),
    map([AnnotatedForm | Acc], T, Fun);
map(Acc, [{func, Context = #{params := Params, exprs := Exprs}} | T], Fun) ->
    AnnotatedParams = map(Params, Fun),
    AnnotatedExprs = map(Exprs, Fun),
    AnnotatedForm = Fun({func, Context#{params => AnnotatedParams, exprs => AnnotatedExprs}}),
    map([AnnotatedForm | Acc], T, Fun);
map(Acc, [{list_lit, Context = #{elements := Elements}} | T], Fun) ->
    AnnotatedElements = map(Elements, Fun),
    AnnotatedForm = Fun({list_lit, Context#{elements => AnnotatedElements}}),
    map([AnnotatedForm | Acc], T, Fun);
map(Acc, [{match, Context = #{left := Left, right := Right}} | T], Fun) ->
    AnnotatedLeft = Fun(Left),
    AnnotatedRight = Fun(Right),
    AnnotatedForm = Fun({match, Context#{left => AnnotatedLeft, right => AnnotatedRight}}),
    map([AnnotatedForm | Acc], T, Fun);
map(Acc, [Form | T], Fun) ->
    AnnotatedForm = Fun(Form),
    map([AnnotatedForm | Acc], T, Fun);
map(Acc, [], _Fun) ->
    lists:reverse(Acc).

%% Scope API

%% globals creates a map of function names to func forms for all top-level
%% functions in RufusForms.
-spec globals(rufus_forms()) -> {ok, #{atom() => rufus_forms()}}.
globals(RufusForms) ->
    globals(#{}, RufusForms).

-spec globals(map(), rufus_forms()) -> {ok, #{atom() => rufus_forms()}}.
globals(Acc, [Form = {func, #{spec := Spec}} | T]) ->
    Forms = maps:get(Spec, Acc, []),
    globals(Acc#{Spec => Forms ++ [Form]}, T);
globals(Acc, [_H | T]) ->
    globals(Acc, T);
globals(Acc, []) ->
    {ok, Acc}.