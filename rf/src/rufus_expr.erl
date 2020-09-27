%% rufus_expr annotates forms with type information and performs typechecks to
%% ensure correctness.
-module(rufus_expr).

-include_lib("rufus_type.hrl").

%% API exports

-export([typecheck_and_annotate/1]).

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
    {ok, Globals} = rufus_forms:globals(RufusForms),
    Locals = #{},
    try
        {ok, _Locals, AnnotatedForms} = typecheck_and_annotate(
            Acc,
            Stack,
            Globals,
            Locals,
            RufusForms
        ),
        ok = rufus_forms:each(AnnotatedForms, fun safety_check/1),
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
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_cons(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {func, _Context} | T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_func(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, Locals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {identifier, _Context} | T]) ->
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_identifier(
        Stack,
        Globals,
        Locals,
        Form
    ),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
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
    Form = {binary_op, Context = #{left := Left, right := Right}}
) ->
    BinaryOpStack = [Form | Stack],
    LeftStack = [rufus_form:make_binary_op_left(Form) | BinaryOpStack],
    {ok, Locals, [AnnotatedLeft]} = typecheck_and_annotate([], LeftStack, Globals, Locals, [Left]),
    RightStack = [rufus_form:make_binary_op_right(Form) | BinaryOpStack],
    {ok, Locals, [AnnotatedRight]} = typecheck_and_annotate([], RightStack, Globals, Locals, [Right]),
    AnnotatedForm1 =
        {binary_op, Context#{
            left => AnnotatedLeft,
            right => AnnotatedRight,
            locals => Locals
        }},
    case rufus_type:resolve(Globals, AnnotatedForm1) of
        {ok, TypeForm} ->
            AnnotatedForm2 =
                {binary_op, Context#{
                    left => AnnotatedLeft,
                    right => AnnotatedRight,
                    type => TypeForm
                }},
            {ok, AnnotatedForm2};
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
    {ok, locals(), cons_form()} | no_return().
typecheck_and_annotate_cons(
    Stack,
    Globals,
    Locals,
    Form = {cons, Context = #{head := Head, tail := Tail}}
) ->
    ConsStack = [Form | Stack],
    HeadStack = [rufus_form:make_cons_head(Form) | ConsStack],
    {ok, NewLocals1, [AnnotatedHead]} = typecheck_and_annotate([], HeadStack, Globals, Locals, [
        Head
    ]),
    TailStack = [rufus_form:make_cons_tail(Form) | ConsStack],
    {ok, NewLocals2, [AnnotatedTail]} = typecheck_and_annotate(
        [],
        TailStack,
        Globals,
        NewLocals1,
        [
            Tail
        ]
    ),
    AnnotatedForm1 = {cons, Context#{head => AnnotatedHead, tail => AnnotatedTail}},
    case rufus_type:resolve(Globals, AnnotatedForm1) of
        {ok, TypeForm} ->
            AnnotatedForm2 =
                {cons, Context#{
                    head => AnnotatedHead,
                    tail => AnnotatedTail,
                    type => TypeForm
                }},
            {ok, NewLocals2, AnnotatedForm2};
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
    Form = {func, Context = #{params := Params, exprs := Exprs}}
) ->
    FuncStack = [Form | Stack],
    ParamsStack = [rufus_form:make_func_params(Form) | FuncStack],
    {ok, NewLocals1, AnnotatedParams} = typecheck_and_annotate(
        [],
        ParamsStack,
        Globals,
        Locals,
        Params
    ),
    ExprsStack = [rufus_form:make_func_exprs(Form) | FuncStack],
    {ok, _NewLocals2, AnnotatedExprs} = typecheck_and_annotate(
        [],
        ExprsStack,
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
-spec typecheck_and_annotate_identifier(rufus_stack(), globals(), locals(), identifier_form()) ->
    {ok, locals(), identifier_form()}.
typecheck_and_annotate_identifier(
    Stack,
    Globals,
    Locals,
    Form = {identifier, Context1 = #{spec := Spec}}
) ->
    {ok, AnnotatedForm1} = annotate_locals(Locals, Form),
    case maps:get(Spec, Locals, undefined) of
        undefined ->
            case rufus_type:resolve(Stack, Globals, AnnotatedForm1) of
                {ok, TypeForm} ->
                    {identifier, Context2} = AnnotatedForm1,
                    AnnotatedForm2 = {identifier, Context2#{type => TypeForm}},
                    {ok, NewLocals} = push_local(Locals, AnnotatedForm2),
                    {ok, NewLocals, AnnotatedForm2};
                {error, Reason, Data} ->
                    throw({error, Reason, Data})
            end;
        TypeForm ->
            AnnotatedForm2 = {identifier, Context1#{type => TypeForm}},
            {ok, Locals, AnnotatedForm2}
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
    Form = {list_lit, Context = #{elements := Elements}}
) ->
    ListLitStack = [Form | Stack],
    {ok, NewLocals, AnnotatedElements} = typecheck_and_annotate(
        [],
        ListLitStack,
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
%% Unknown identifiers in the left operand are treated as unbound variables and
%% their type information is inferred from the right operand. Return values:
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The match form and
%%   its operands are annotated with type information.
%% - `{error, unknown_identifier, Data}` is thrown if the right operand is
%%   unbound.
%% - `{error, unmatched_types, Data}` is thrown when the left and right operand
%%   have differing types.
-spec typecheck_and_annotate_match(rufus_stack(), globals(), locals(), match_form()) ->
    {ok, locals(), match_form()} | no_return().
typecheck_and_annotate_match(
    Stack,
    Globals,
    Locals,
    Form = {match, Context = #{left := Left, right := Right}}
) ->
    MatchStack1 = [Form | Stack],
    RightStack = [rufus_form:make_match_right(Form) | MatchStack1],
    {ok, NewLocals1, [AnnotatedRight]} = typecheck_and_annotate(
        [],
        RightStack,
        Globals,
        Locals,
        [Right]
    ),
    AnnotatedForm1 = {match, Context#{right => AnnotatedRight}},

    MatchStack2 = [AnnotatedForm1 | Stack],
    LeftStack = [rufus_form:make_match_left(Form) | MatchStack2],
    {ok, NewLocals2, [AnnotatedLeft]} = typecheck_and_annotate(
        [],
        LeftStack,
        Globals,
        NewLocals1,
        [Left]
    ),

    case rufus_form:type_spec(AnnotatedLeft) == rufus_form:type_spec(AnnotatedRight) of
        true ->
            AnnotatedForm2 =
                {match, Context#{
                    left => AnnotatedLeft,
                    right => AnnotatedRight,
                    type => rufus_form:type(AnnotatedRight)
                }},
            ok = validate_pattern(Globals, NewLocals2, AnnotatedForm2),
            {ok, NewLocals2, AnnotatedForm2};
        false ->
            Data = #{
                globals => Globals,
                locals => Locals,
                left => AnnotatedLeft,
                right => AnnotatedRight
            },
            throw({error, unmatched_types, Data})
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
validate_pattern(_Data, {list_lit, _Context}) ->
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
