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
-spec typecheck_and_annotate(rufus_forms()) -> {ok, rufus_forms()} | error_triple().
typecheck_and_annotate(RufusForms) ->
    Acc = [],
    Stack = [],
    {ok, Globals} = rufus_form:globals(RufusForms),
    Locals = #{},
    try
        {ok, _Locals, AnnotatedForms} = typecheck_and_annotate(
            Acc,
            Stack,
            Globals,
            Locals,
            RufusForms
        ),
        ok = rufus_form:each(AnnotatedForms, fun safety_check/1),
        {ok, AnnotatedForms}
    catch
        {error, Code, Data} ->
            {error, Code, Data}
    end.

%% Private API

%% safety_check ensures that every form has type information. An `{error,
%% safety_check, Data}` error triple is thrown if a form doesn't have type
%% information, otherwise `ok` is returned.
-spec safety_check(rufus_form()) -> ok | no_return().
safety_check({func, _Context}) ->
    ok;
safety_check({module, _Context}) ->
    ok;
safety_check({_FormType, #{type := _Type}}) ->
    ok;
safety_check(Form) ->
    Data = #{
        form => Form,
        error => missing_type_information
    },
    throw({error, safety_check, Data}).

%% typecheck_and_annotate iterates over RufusForms and adds type information
%% from the current scope to each form. An `{error, Reason, Data}` error triple
%% is thrown at the first error.
-spec typecheck_and_annotate(rufus_forms(), rufus_stack(), globals(), locals(), rufus_forms()) ->
    {ok, locals(), rufus_forms()}.
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {binary_op, _Context} | T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_binary_op(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, Locals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {call, _Context} | T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_call(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, Locals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {cons, _Context} | T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_cons(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, Locals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {func, _Context} | T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_func(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, Locals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {identifier, _Context} | T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_identifier(Stack, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, Locals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {list_lit, _Context} | T]) ->
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_list_lit(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {match, _Context} | T]) ->
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_match(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {param, _Context} | T]) ->
    {ok, NewLocals} = push_local(Locals, Form),
    typecheck_and_annotate([Form | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [H | T]) ->
    typecheck_and_annotate([H | Acc], Stack, Globals, Locals, T);
typecheck_and_annotate(Acc, _Stack, _Globals, Locals, []) ->
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

%% binary_op helpers

%% typecheck_and_annotate_binary_op ensures that binary_op operands are
%% exclusively ints or exclusively floats. Inferred type information is added to
%% every `binary_op` form. Return values:
%% - `{ok, AnnotatedForms}` if no issues are found. Every `binary_op` form is
%%   annotated with type information.
%% - `{error, unmatched_operand_type, Form}` is thrown if an `int` operand is
%%   mixed with a `float` operand. `Form` contains the illegal operands.
%% - `{error, unsupported_operand_type, Form}` is thrown if a type other than an
%%   int is used as an operand. `Form` contains the illegal operands.
-spec typecheck_and_annotate_binary_op(rufus_stack(), globals(), locals(), binary_op_form()) ->
    {ok, binary_op_form()} | no_return().
typecheck_and_annotate_binary_op(
    Stack,
    Globals,
    Locals,
    {binary_op, Context = #{left := Left, right := Right}}
) ->
    {ok, Locals, [AnnotatedLeft]} = typecheck_and_annotate([], Stack, Globals, Locals, [Left]),
    {ok, Locals, [AnnotatedRight]} = typecheck_and_annotate([], Stack, Globals, Locals, [Right]),
    Form = {binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight, locals => Locals}},
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm =
                {binary_op, Context#{
                    left => AnnotatedLeft,
                    right => AnnotatedRight,
                    type => TypeForm
                }},
            {ok, AnnotatedForm};
        Error ->
            throw(Error)
    end.

%% call helpers

%% typecheck_and_annotate_call resolves the return type for a function call and
%% returns a call form annotated with type information.
-spec typecheck_and_annotate_call(rufus_stack(), globals(), locals(), call_form()) ->
    {ok, call_form()} | no_return().
typecheck_and_annotate_call(Stack, Globals, Locals, {call, Context1 = #{args := Args}}) ->
    {ok, _NewLocals, AnnotatedArgs} = typecheck_and_annotate([], Stack, Globals, Locals, Args),
    Form = {call, Context2 = Context1#{args => AnnotatedArgs}},
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = {call, Context2#{type => TypeForm}},
            {ok, AnnotatedForm};
        Error ->
            throw(Error)
    end.

%% cons helpers

%% typecheck_and_annotate_cons enforces the constraint that the head and tail
%% elements are of the expected type. Return values:
%% - `{ok, AnnotatedForm}` if no issues are found.
%% - `{error, unexpected_element_type, Data}` is thrown if either the head or
%%   tail elements have type issues.
-spec typecheck_and_annotate_cons(rufus_stack(), globals(), locals(), cons_form()) ->
    {ok, cons_form()} | no_return().
typecheck_and_annotate_cons(
    Stack,
    Globals,
    Locals,
    {cons, Context = #{head := Head, tail := Tail}}
) ->
    {ok, NewLocals1, [AnnotatedHead]} = typecheck_and_annotate([], Stack, Globals, Locals, [Head]),
    {ok, _NewLocals2, [AnnotatedTail]} = typecheck_and_annotate([], Stack, Globals, NewLocals1, [
        Tail
    ]),
    AnnotatedForm1 = {cons, Context#{head => AnnotatedHead, tail => AnnotatedTail}},
    case rufus_type:resolve(Globals, AnnotatedForm1) of
        {ok, TypeForm} ->
            AnnotatedForm2 =
                {cons, Context#{
                    head => AnnotatedHead,
                    tail => AnnotatedTail,
                    type => TypeForm
                }},
            {ok, AnnotatedForm2};
        Error ->
            throw(Error)
    end.

%% func helpers

%% typecheck_and_annotate_func adds all parameters to the local scope. It also
%% resolves and annotates types for all expressions in the function body to
%% ensure they satisfy type constraints.
-spec typecheck_and_annotate_func(rufus_stack(), globals(), locals(), func_form()) ->
    {ok, func_form()} | no_return().
typecheck_and_annotate_func(
    Stack,
    Globals,
    Locals,
    {func, Context = #{params := Params, exprs := Exprs}}
) ->
    {ok, NewLocals1, AnnotatedParams} = typecheck_and_annotate([], Stack, Globals, Locals, Params),
    {ok, _NewLocals2, AnnotatedExprs} = typecheck_and_annotate(
        [],
        Stack,
        Globals,
        NewLocals1,
        Exprs
    ),
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

%% identifier helpers

%% typecheck_and_annotate_identifier adds a locals key/value pair to the
%% identifier with information about local variables that are in scope. Type
%% information is also added to the identifier form if present in Locals. Return
%% values:
%% - `{ok, AnnotatedForm}` with locals and type information..
-spec typecheck_and_annotate_identifier(rufus_stack(), locals(), identifier_form()) ->
    {ok, identifier_form()}.
typecheck_and_annotate_identifier(_Stack, Locals, Form = {identifier, Context = #{spec := Spec}}) ->
    {ok, AnnotatedForm1} = annotate_locals(Locals, Form),
    case maps:get(Spec, Locals, undefined) of
        undefined ->
            {ok, AnnotatedForm1};
        Type ->
            AnnotatedForm2 = {identifier, Context#{type => Type}},
            {ok, AnnotatedForm2}
    end.

%% list_lit helpers

%% typecheck_and_annotate_list_lit enforces the constraint that each list
%% element matches the collection type. Returns values:
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The list_lit form and
%%   its elements are annotated with type information.
%% - `{error, unexpected_element_type, Data}` is thrown if an element is found
%%   with a differing type.
-spec typecheck_and_annotate_list_lit(rufus_stack(), globals(), locals(), list_lit_form()) ->
    {ok, locals(), list_lit_form()} | no_return().
typecheck_and_annotate_list_lit(
    Stack,
    Globals,
    Locals,
    {list_lit, Context = #{elements := Elements}}
) ->
    {ok, NewLocals, AnnotatedElements} = typecheck_and_annotate(
        [],
        Stack,
        Globals,
        Locals,
        Elements
    ),
    AnnotatedForm1 = {list_lit, Context#{elements => AnnotatedElements}},
    case rufus_type:resolve(Globals, AnnotatedForm1) of
        {ok, TypeForm} ->
            AnnotatedForm2 = {list_lit, Context#{elements => AnnotatedElements, type => TypeForm}},
            {ok, NewLocals, AnnotatedForm2};
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
-spec typecheck_and_annotate_match(rufus_stack(), globals(), locals(), match_form()) ->
    {ok, locals(), match_form()} | no_return().
typecheck_and_annotate_match(Stack, Globals, Locals, {match, Context = #{left := Left}}) ->
    {ok, NewLocals1, [AnnotatedLeft1]} = typecheck_and_annotate([], Stack, Globals, Locals, [Left]),
    AnnotatedForm1 = {match, Context#{left => AnnotatedLeft1}},
    case rufus_form:has_type(AnnotatedLeft1) of
        true ->
            ok = validate_pattern(Globals, NewLocals1, AnnotatedForm1),
            typecheck_and_annotate_match_with_bound_left_operand(
                Stack,
                Globals,
                NewLocals1,
                AnnotatedForm1
            );
        false ->
            typecheck_and_annotate_match_with_unbound_left_operand(
                Stack,
                Globals,
                NewLocals1,
                AnnotatedForm1
            )
    end.

%% typecheck_and_annotate_match_with_bound_left_operand typechecks match
%% expressions with a left operand that has a known type. Return values:
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The match form and
%%   its operands are annotated with type information.
%% - `{error, unbound_variable, Data}` is thrown if the right operand is
%%   unbound.
%% - `{error, unmarched_types, Data}` is thrown when the left and right operand
%%   have differing types.
-spec typecheck_and_annotate_match_with_bound_left_operand(
    rufus_stack(),
    globals(),
    locals(),
    match_form()
) -> {ok, locals(), match_form()} | no_return().
typecheck_and_annotate_match_with_bound_left_operand(
    Stack,
    Globals,
    Locals,
    {match, Context = #{left := Left, right := Right}}
) ->
    try
        {ok, NewLocals, [AnnotatedRight]} = typecheck_and_annotate([], Stack, Globals, Locals, [
            Right
        ]),
        case rufus_form:type_spec(Left) == rufus_form:type_spec(AnnotatedRight) of
            true ->
                RightType = rufus_form:type(AnnotatedRight),
                AnnotatedForm =
                    {match, Context#{
                        left => Left,
                        right => AnnotatedRight,
                        type => RightType
                    }},
                {ok, NewLocals, AnnotatedForm};
            false ->
                case AnnotatedRight of
                    {identifier, _Context2} ->
                        case rufus_form:has_type(AnnotatedRight) of
                            false ->
                                Data2 = #{
                                    globals => Globals,
                                    locals => Locals,
                                    form => AnnotatedRight
                                },
                                throw({error, unbound_variable, Data2});
                            true ->
                                ok
                        end;
                    _ ->
                        ok
                end,

                Data3 = #{
                    globals => Globals,
                    locals => Locals,
                    left => Left,
                    right => AnnotatedRight
                },
                throw({error, unmatched_types, Data3})
        end
    catch
        {error, unknown_identifier, Data1} ->
            throw({error, unbound_variable, Data1})
    end.

%% typecheck_and_annotate_match_with_unbound_left_operand typechecks match
%% expressions with a left operand that does not have a known type. Return
%% values:
%% - `{ok, NewLocals, AnnotatedForm}` if no issues are found. The match form
%%   and its operands are annotated with type information.
%% - `{error, unbound_variables, Data}` is thrown if both the left and right
%%   operands are unbound.
-spec typecheck_and_annotate_match_with_unbound_left_operand(
    rufus_stack(),
    globals(),
    locals(),
    match_form()
) -> {ok, locals(), match_form()} | no_return().
typecheck_and_annotate_match_with_unbound_left_operand(
    Stack,
    Globals,
    Locals,
    {match, Context = #{left := Left, right := Right}}
) ->
    {ok, NewLocals1, [AnnotatedRight]} = typecheck_and_annotate([], Stack, Globals, Locals, [Right]),
    case rufus_form:has_type(AnnotatedRight) of
        true ->
            {LeftType, LeftContext} = Left,
            RightType = rufus_form:type(AnnotatedRight),
            AnnotatedLeft1 = {LeftType, LeftContext#{type => RightType}},
            {ok, NewLocals2} = push_local(NewLocals1, AnnotatedLeft1),
            {ok, AnnotatedLeft2} = annotate_locals(NewLocals2, AnnotatedLeft1),
            AnnotatedForm =
                {match, Context#{
                    left => AnnotatedLeft2,
                    right => AnnotatedRight,
                    type => RightType
                }},
            {ok, NewLocals2, AnnotatedForm};
        false ->
            Data = #{
                globals => Globals,
                locals => Locals,
                left => Left,
                right => AnnotatedRight
            },
            throw({error, unbound_variables, Data})
    end.

%% validate_pattern checks the left hand side of a pattern match expression for
%% valid expressions. An `{error, illegal_pattern, Data}` error triple is thrown
%% if an invalid expression is found.
%%
%% TODO(jkakar) Figure out why Dialyzer doesn't like this spec:
%% -spec validate_pattern(globals(), locals(), match_form()) -> ok | no_return().
validate_pattern(Globals, Locals, Form = {match, _Context}) ->
    Data = #{
        globals => Globals,
        locals => Locals,
        form => Form
    },
    validate_pattern(Data, Form).

%% validate_pattern inspects the left hand operand of a match form to ensure
%% that its a valid pattern. A pattern has the same structure as a term but can
%% contain unbound variables. An `{error, illegal_pattern, Data}` error triple
%% is thrown if the left hand operand contains unsupported expressions.
-spec validate_pattern(context(), rufus_form()) -> ok | no_return().
validate_pattern(Data, {match, #{left := Left}}) ->
    validate_pattern(Data, Left);
validate_pattern(_Data, {atom_lit, _Context}) ->
    ok;
validate_pattern(_Data, {bool_lit, _Context}) ->
    ok;
validate_pattern(_Data, {float_lit, _Context}) ->
    ok;
validate_pattern(_Data, {int_lit, _Context}) ->
    ok;
validate_pattern(_Data, {string_lit, _Context}) ->
    ok;
validate_pattern(_Data, {identifier, _Context}) ->
    ok;
validate_pattern(Data, Form = {binary_op, _Context}) ->
    case is_constant_expr(Form) of
        true ->
            ok;
        false ->
            throw({error, illegal_pattern, Data})
    end;
validate_pattern(Data, _Form) ->
    throw({error, illegal_pattern, Data}).

%% is_constant_expr returns true if a binary_op can be evaluated to a constant
%% during compilation. Otherwise, it returns false.
-spec is_constant_expr(rufus_form()) -> boolean().
is_constant_expr({binary_op, #{left := Left, right := Right}}) ->
    is_constant_expr(Left) and is_constant_expr(Right);
is_constant_expr({float_lit, _Context}) ->
    true;
is_constant_expr({int_lit, _Context}) ->
    true;
is_constant_expr(_Form) ->
    false.
