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
        {ok, AnnotatedForms}
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

%% typecheck_and_annotate iterates over RufusForms and adds type information
%% from the current scope to each form. An `{error, Reason, Data}` error triple
%% is thrown at the first error.
-spec typecheck_and_annotate(list(rufus_form()), globals(), locals(), list(rufus_form())) -> {ok, locals(), list(rufus_form())}.
typecheck_and_annotate(Acc, Globals, Locals, [Form = {func, _Context}|T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_func(Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, Globals, Locals, [Form = {param, _Context}|T]) ->
    {ok, NewLocals} = push_local(Locals, Form),
    typecheck_and_annotate([Form|Acc], Globals, NewLocals, T);
typecheck_and_annotate(Acc, Globals, Locals, [Form = {binary_op, _Context}|T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_binary_op(Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, Globals, Locals, [Form = {identifier, _Context}|T]) ->
    {ok, AnnotatedForm} = annotate_locals(Locals, Form),
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, Globals, Locals, [Form = {call, _Context}|T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_call(Globals, Form),
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, Globals, Locals, [Form = {match, _Context}|T]) ->
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_match(Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, NewLocals, T);
typecheck_and_annotate(Acc, Globals, Locals, [H|T]) ->
    typecheck_and_annotate([H|Acc], Globals, Locals, T);
typecheck_and_annotate(Acc, _Globals, Locals, []) ->
    {ok, Locals, lists:reverse(Acc)}.

%% scope helpers

%% annotate_locals adds a `locals` key to a form context.
-spec annotate_locals(locals(), rufus_form()) -> {ok, rufus_form()}.
annotate_locals(Locals, {FormType, Context}) ->
    {ok, {FormType, Context#{locals => Locals}}}.

%% push_local adds a form to the local scope.
-spec push_local(locals(), rufus_form()) -> {ok, locals()}.
push_local(Locals, {_FormType, #{spec := Spec, type := Type}}) ->
    {ok, Locals#{Spec => Type}}.

%% func helpers

%% typecheck_and_annotate_func adds all parameters to the local scope. It also
%% resolves and annotates types for all expressions in the function body to
%% ensure they satisfy type constraints.
-spec typecheck_and_annotate_func(globals(), locals(), func_form()) -> {ok, func_form()} | no_return().
typecheck_and_annotate_func(Globals, Locals, {func, Context = #{params := Params, exprs := Exprs}}) ->
    {ok, NewLocals1, AnnotatedParams} = typecheck_and_annotate([], Globals, Locals, Params),
    {ok, _NewLocals2, AnnotatedExprs} = typecheck_and_annotate([], Globals, NewLocals1, Exprs),
    AnnotatedForm = {func, Context#{params => AnnotatedParams, exprs => AnnotatedExprs}},
    ok = typecheck_func_return_type(Globals, AnnotatedForm),
    {ok, AnnotatedForm}.

%% typecheck_func_return_type enforces the constraint that the type of the final
%% expression in a function matches its return type. `ok` is returned if
%% typechecks all pass, otherwise an `{error, unmatched_return_type, Data}`
%% error triple is thrown.
-spec typecheck_func_return_type(globals(), func_form()) -> ok | no_return().
typecheck_func_return_type(Globals, {func, #{return_type := ReturnType, exprs := Exprs}}) ->
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
    end.

%% call helpers

%% typecheck_and_annotate_call resolves the return type for a function call and
%% returns a call form annotated with type information.
%%
%% TODO(jkakar) Figure out why Dialyzer doesn't like this spec:
%% -spec typecheck_and_annotate_call(globals(), call_form()) -> {ok, call_form()} | no_return().
typecheck_and_annotate_call(Globals, Form = {call, Context}) ->
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = {call, Context#{type => TypeForm}},
            {ok, AnnotatedForm};
        Error ->
            throw(Error)
    end.

%% binary_op helpers

%% typecheck_and_annotate_binary_op ensures that binary_op operands are
%% exclusively ints or exclusively floats. Inferred type information is added to
%% every `binary_op` form. Returns values:
%% - `{ok, AnnotatedForms}` if no issues are found. Every `binary_op` form is
%%   annotated with an inferred type annotation.
%% - `{error, unmatched_operand_type, Form}` is thrown if an `int` operand is
%%   mixed with a `float` operand. `Form` contains the illegal operands.
%% - `{error, unsupported_operand_type, Form}` is thrown if a type other than an
%%   int is used as an operand. `Form` contains the illegal operands.
-spec typecheck_and_annotate_binary_op(globals(), locals(), binary_op_form()) -> {ok, binary_op_form()} | no_return().
typecheck_and_annotate_binary_op(Globals, Locals, {binary_op, Context = #{left := Left, right := Right}}) ->
    {ok, Locals, [AnnotatedLeft]} = typecheck_and_annotate([], Globals, Locals, [Left]),
    {ok, Locals, [AnnotatedRight]} = typecheck_and_annotate([], Globals, Locals, [Right]),
    Form = {binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight, locals => Locals}},
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = {binary_op, Context#{left => AnnotatedLeft,
                                                 right => AnnotatedRight,
                                                 type => TypeForm}},
            {ok, AnnotatedForm};
        Error ->
            throw(Error)
    end.

%% match helpers

%% typecheck_and_annotate_match ensures that match operands have matching types.
%% The left operand is treated as unbound variable when no information is known
%% about it, and it's added to the local scope with type information from the
%% right operand. Return values:
%%
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The match form and
%%   its operands are annotated with type information.
%% - `{error, unbound_variable, Data}` is thrown if the right operand is
%%   unbound.
%% - `{error, unbound_variables, Data}` is thrown if both the left and right
%%   operands are unbound.
%% - `{error, unmarched_types, Data}` is thrown when the left and right operand
%%   have differing types.
-spec typecheck_and_annotate_match(globals(), locals(), match_form()) -> {ok, locals(), match_form()} | no_return().
typecheck_and_annotate_match(Globals, Locals, Form = {match, Context = #{left := Left, right := Right}}) ->
    case resolve_match_operand_type(Globals, Locals, Left) of
        {ok, NewLocals, AnnotatedLeft} ->
            AnnotatedForm = {match, Context#{left => AnnotatedLeft, right => Right}},
            typecheck_and_annotate_match_with_known_left_operand(Globals, NewLocals, AnnotatedForm);
        {error, unknown_identifier, LeftData} ->
            typecheck_and_annotate_match_with_unknown_left_operand(Globals, Locals, Form, LeftData)
    end.

typecheck_and_annotate_match_with_known_left_operand(Globals, Locals, {match, Context = #{left := Left, right := Right}}) ->
    case resolve_match_operand_type(Globals, Locals, Right) of
        {ok, NewLocals, AnnotatedRight} ->
            case rufus_form:type_spec(Left) == rufus_form:type_spec(AnnotatedRight) of
                true ->
                    RightType = rufus_form:type(AnnotatedRight),
                    AnnotatedForm = {match, Context#{left => Left,
                                                     right => AnnotatedRight,
                                                     type => RightType}},
                    {ok, NewLocals, AnnotatedForm};
                false ->
                    Data = #{globals => Globals,
                             locals => Locals,
                             left => Left,
                             right => AnnotatedRight},
                    throw({error, unmatched_types, Data})
            end;
        {error, unknown_identifier, Data} ->
            throw({error, unbound_variable, Data})
    end.

typecheck_and_annotate_match_with_unknown_left_operand(Globals, Locals, {match, Context = #{left := Left, right := Right}}, LeftData) ->
    case resolve_match_operand_type(Globals, Locals, Right) of
        {ok, NewLocals1, AnnotatedRight} ->
            {LeftType, LeftContext} = Left,
            RightType = rufus_form:type(AnnotatedRight),
            AnnotatedLeft = {LeftType, LeftContext#{type => RightType}},
            AnnotatedForm = {match, Context#{left => AnnotatedLeft,
                                             right => AnnotatedRight,
                                             type => RightType}},
            {ok, NewLocals2} = push_local(NewLocals1, AnnotatedLeft),
            {ok, NewLocals2, AnnotatedForm};
        {error, unknown_identifier, RightData} ->
            throw({error, unbound_variables, #{left => LeftData, right => RightData}})
    end.

resolve_match_operand_type(Globals, Locals, Form = {FormType, Context}) ->
    {ok, AnnotatedForm} = annotate_locals(Locals, Form),
    case rufus_type:resolve(Globals, AnnotatedForm) of
        {ok, TypeForm} ->
            {ok, Locals, {FormType, Context#{type => TypeForm}}};
        {error, unknown_identifier, Data} ->
            {error, unknown_identifier, Data};
        Error ->
            throw(Error)
    end.
