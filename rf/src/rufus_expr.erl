%% rufus_expr annotates forms with type information and performs typechecks to
%% ensure correctness.
-module(rufus_expr).

-include_lib("rufus_type.hrl").

%% API exports

-export([
    typecheck_and_annotate/1
]).

%% API

%% typecheck_and_annotate iterates over RufusForms and adds type information
%% from the current scope to each form. Iteration stops at the first error.
%% Return values:
%% - `{ok, AnnotatedRufusForms}` if no issues are found.
%% - `{error, unknown_func, Data}` with `Data` containing a `spec` key that has
%%   the function name.
%% - `{error, incorrect_arg_count, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to the number of args received and the number
%%   of args expected, respectively
%% - `{error, invalid_arg_type, Data}` with `Data` containing `actual` and
%%   `expected` atom keys pointing to Rufus types if return value types are
%%   unmatched.
typecheck_and_annotate(RufusForms) ->
    {ok, Globals} = rufus_form:globals(RufusForms),
    try
        {ok, _Locals, AnnotatedForms} = typecheck_and_annotate([], Globals, #{}, RufusForms),
        %% Function return type typechecks need to happen in a second pass
        %% because they depend on the first pass to add type annotations to all
        %% forms.
        ok = typecheck_return_type(Globals, AnnotatedForms),
        {ok, AnnotatedForms}
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

%% typecheck_and_annotate iterates over RufusForms and adds type information
%% from the current scope to each form. An `{error, Reason, Data}` error triple
%% is thrown at the first error.
-spec typecheck_and_annotate(list(rufus_form()), globals(), locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
typecheck_and_annotate(Acc, Globals, Locals, [{func_decl, Context = #{params := Params, exprs := Exprs}}|T]) ->
    {ok, NewLocals1, AnnotatedParams} = typecheck_and_annotate([], Globals, Locals, Params),
    {ok, NewLocals2, AnnotatedExprs} = typecheck_and_annotate([], Globals, NewLocals1, Exprs),
    AnnotatedForm = {func_decl, Context#{params => AnnotatedParams, exprs => AnnotatedExprs}},
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, NewLocals2, T);
typecheck_and_annotate(Acc, Globals, Locals, [{param, Context = #{spec := Spec, type := Type}}|T]) ->
    NewLocals = Locals#{Spec => Type},
    AnnotatedForm = {param, Context},
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, NewLocals, T);
typecheck_and_annotate(Acc, Globals, Locals, [{binary_op, Context = #{left := Left, right := Right}}|T]) ->
    {ok, Locals, [AnnotatedLeft]} = typecheck_and_annotate([], Globals, Locals, [Left]),
    {ok, Locals, [AnnotatedRight]} = typecheck_and_annotate([], Globals, Locals, [Right]),
    Form = {binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight, locals => Locals}},
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = {binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight, type => TypeForm}},
            typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
        Error ->
            throw(Error)
    end;
typecheck_and_annotate(Acc, Globals, Locals, [{identifier, Context}|T]) ->
    AnnotatedForm = {identifier, Context#{locals => Locals}},
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, Globals, Locals, [Form = {call, _Context}|T]) ->
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = rufus_form:annotate(Form, type, TypeForm),
            typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
        Error ->
            throw(Error)
    end;
typecheck_and_annotate(Acc, Globals, Locals, [H|T]) ->
    typecheck_and_annotate([H|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, _Globals, Locals, []) ->
    {ok, Locals, lists:reverse(Acc)}.

%% typecheck_return_type enforces the constraint that the type of the final
%% expression in a function matches its return type. `ok` is returned if
%% typechecks all pass, otherwise an `{error, Reason, Data}` error triple is
%% thrown.
typecheck_return_type(Globals, [{func_decl, #{return_type := ReturnType, exprs := Exprs}}|T]) ->
    LastExpr = lists:last(Exprs),
    case rufus_type:resolve(Globals, LastExpr) of
        {ok, {type, #{spec := ActualSpec}}} ->
            {type, #{spec := ExpectedSpec}} = ReturnType,
            case ExpectedSpec == ActualSpec of
                true ->
                    ok;
                false ->
                    Data = #{return_type => ReturnType, expr => LastExpr},
                    throw({error, unmatched_return_type, Data})
            end;
        Error ->
            throw(Error)
    end,
    typecheck_return_type(Globals, T);
typecheck_return_type(Globals, [_H|T]) ->
    typecheck_return_type(Globals, T);
typecheck_return_type(_Globals, []) ->
    ok.
