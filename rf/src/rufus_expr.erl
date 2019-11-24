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
%% - `{ok, AnnotatedForms}` if no issues are found.
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
        {error, Code, Data} ->
            {error, Code, Data}
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
%% When the left operand is an identifier it's treated as an unbound variable if
%% no type information is available. It's added to the local scope with type
%% information inferred from the right operand. Return values:
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The match form and
%%   its operands are annotated with type information.
%% - `{error, unbound_variable, Data}` is thrown if the right operand is
%%   unbound.
%% - `{error, unbound_variables, Data}` is thrown if both the left and right
%%   operands are unbound.
%% - `{error, unmarched_types, Data}` is thrown when the left and right operand
%%   have differing types.
-spec typecheck_and_annotate_match(globals(), locals(), match_form()) -> {ok, locals(), match_form()} | no_return().
typecheck_and_annotate_match(Globals, Locals, {match, Context = #{left := Left}}) ->
    {ok, NewLocals1, [AnnotatedLeft1]} = typecheck_and_annotate([], Globals, Locals, [Left]),
    AnnotatedForm1 = {match, Context#{left => AnnotatedLeft1}},
    case rufus_form:has_type(AnnotatedLeft1) of
        true ->
            ok = validate_pattern(Globals, NewLocals1, AnnotatedForm1),
            typecheck_and_annotate_match_with_bound_left_operand(Globals, NewLocals1, AnnotatedForm1);
        false ->
            typecheck_and_annotate_match_with_unbound_left_operand(Globals, NewLocals1, AnnotatedForm1)
    end.

%% typecheck_and_annotate_match_with_bound_left_operand typechecks match
%% expressions with a left operand that has a known type. Return values:
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The match form and
%%   its operands are annotated with type information.
%% - `{error, unbound_variable, Data}` is thrown if the right operand is
%%   unbound.
%% - `{error, unmarched_types, Data}` is thrown when the left and right operand
%%   have differing types.
-spec typecheck_and_annotate_match_with_bound_left_operand(globals(), locals(), match_form()) -> {ok, locals(), match_form()} | no_return().
typecheck_and_annotate_match_with_bound_left_operand(Globals, Locals, {match, Context = #{left := Left, right := Right}}) ->
    try
        {ok, NewLocals, [AnnotatedRight]} = typecheck_and_annotate([], Globals, Locals, [Right]),
        case rufus_form:type_spec(Left) == rufus_form:type_spec(AnnotatedRight) of
            true ->
                RightType = rufus_form:type(AnnotatedRight),
                AnnotatedForm = {match, Context#{left => Left,
                                                 right => AnnotatedRight,
                                                 type => RightType}},
                {ok, NewLocals, AnnotatedForm};
            false ->
                case AnnotatedRight of
                    {identifier, _Context2} ->
                        case rufus_form:has_type(AnnotatedRight) of
                            false ->
                                Data2 = #{globals => Globals,
                                          locals => Locals,
                                          form => AnnotatedRight},
                                throw({error, unbound_variable, Data2});
                            true ->
                                ok
                        end;
                    _ ->
                        ok
                end,

                Data3 = #{globals => Globals,
                          locals => Locals,
                          left => Left,
                          right => AnnotatedRight},
                throw({error, unmatched_types, Data3})
        end
    catch {error, unknown_identifier, Data1} ->
        throw({error, unbound_variable, Data1})
    end.

%% typecheck_and_annotate_match_with_unbound_left_operand typechecks match
%% expressions with a left operand that does not have a known type. Return
%% values:
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The match form and
%%   its operands are annotated with type information.
%% - `{error, unbound_variables, Data}` is thrown if both the left and right
%%   operands are unbound.
-spec typecheck_and_annotate_match_with_unbound_left_operand(globals(), locals(), match_form()) -> {ok, locals(), match_form()} | no_return().
typecheck_and_annotate_match_with_unbound_left_operand(Globals, Locals, {match, Context = #{left := Left, right := Right}}) ->
    {ok, NewLocals1, [AnnotatedRight]} = typecheck_and_annotate([], Globals, Locals, [Right]),
    case rufus_form:has_type(AnnotatedRight) of
        true ->
            {LeftType, LeftContext} = Left,
            RightType = rufus_form:type(AnnotatedRight),
            AnnotatedLeft1 = {LeftType, LeftContext#{type => RightType}},
            {ok, NewLocals2} = push_local(NewLocals1, AnnotatedLeft1),
            {ok, AnnotatedLeft2} = annotate_locals(NewLocals2, AnnotatedLeft1),
            AnnotatedForm = {match, Context#{left => AnnotatedLeft2,
                                             right => AnnotatedRight,
                                             type => RightType}},
            {ok, NewLocals2, AnnotatedForm};
        false ->
            Data = #{globals => Globals,
                     locals => Locals,
                     left => Left,
                     right => AnnotatedRight},
            throw({error, unbound_variables, Data})
    end.

%% validate_pattern checks the left hand side of a pattern match expression for
%% call expressions. An `{error, illegal_pattern, Data}` error triple is thrown
%% if one is found.
%%
%% TODO(jkakar) Figure out why Dialyzer doesn't like this spec:
%% -spec validate_pattern(globals(), locals(), match_form()) -> ok | no_return().
validate_pattern(Globals, Locals, Form = {match, _Context}) ->
    Data = #{globals => Globals,
             locals => Locals,
             form => Form},
    validate_pattern(Data, Form).

%% validate_pattern recursively searches the left hand operand for signs of a
%% call expession. An `{error, illegal_pattern, Data}` error triple is thrown if
%% one is found.
-spec validate_pattern(context(), rufus_form()) -> ok | no_return().
validate_pattern(Data, {match, #{left := Left}}) ->
    validate_pattern(Data, Left);
validate_pattern(Data, {call, _Context}) ->
    throw({error, illegal_pattern, Data});
validate_pattern(Data, {binary_op, #{left := Left, right := Right}}) ->
    validate_pattern(Data, Left),
    validate_pattern(Data, Right);
validate_pattern(_Data, _Form) ->
    ok.
