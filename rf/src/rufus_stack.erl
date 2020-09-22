%% rufus_stack reports on location information for a form, such as whether it's
%% part of a function parameter list, for example.
-module(rufus_stack).

-include_lib("rufus_type.hrl").

%% API exports

-export([
    is_param/1,
    push/2
]).

%% API

%% is_param returns true if Stack contains a params form, and therefore whether
%% the current node is part of a function's parameter list.
-spec is_param(rufus_stack()) -> boolean().
is_param(Stack) ->
    Fun = fun({params, _Context}) ->
            true;
        (_) ->
            false
    end,
    lists:any(Fun, Stack).

%% push adds a form to the top of the stack.
-spec push(rufus_stack(), rufus_form()) -> rufus_stack().
push(Stack, Form) ->
    [Form|Stack].
