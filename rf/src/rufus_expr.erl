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
    io:format("~n", []),
    Acc = [],
    Stack = [],
    Locals = #{},
    try
        {ok, Globals, AnnotatedForms1} = typecheck_and_annotate_globals(Acc, Stack, RufusForms),
        {ok, _Locals, AnnotatedForms2} = typecheck_and_annotate(
            Acc,
            Stack,
            Globals,
            Locals,
            AnnotatedForms1
        ),
        ok = rufus_forms:each(AnnotatedForms2, fun safety_check/1),
        {ok, AnnotatedForms2}
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

%% typecheck_and_annotate_globals iterates over RufusForms and adds type
%% information to all module-level functions. An `{error, Reason, Data}` error
%% triple is thrown at the first error.
-spec typecheck_and_annotate_globals(rufus_forms(), rufus_stack(), rufus_forms()) ->
    {ok, globals(), rufus_forms()}.
typecheck_and_annotate_globals(Acc, Stack, [Form = {func, _Context} | T]) ->
    {ok, AnnotatedForm} = typecheck_and_annotate_func_params(Stack, Form),
    typecheck_and_annotate_globals([AnnotatedForm | Acc], Stack, T);
typecheck_and_annotate_globals(Acc, Stack, [Form = {module, _Context} | T]) ->
    typecheck_and_annotate_globals([Form | Acc], Stack, T);
typecheck_and_annotate_globals(Acc, _Stack, []) ->
    Forms = lists:reverse(Acc),
    {ok, Globals} = rufus_forms:globals(Forms),
    {ok, Globals, Forms}.

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
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {catch_clause, _Context} | T]) ->
    io:format("Form => ~p~n", [Form]),
    {ok, AnnotatedForm} = typecheck_and_annotate_catch_clause(Stack, Globals, Locals, Form),
    io:format("AnnotatedForm => ~p~n", [AnnotatedForm]),
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
    io:format("AnnotatedForm identifier => ~p~n", [AnnotatedForm]),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {list_lit, _Context} | T]) ->
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_list_lit(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {match_op, _Context} | T]) ->
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_match_op(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {param, _Context} | T]) ->
    {ok, NewLocals} = push_local(Locals, Form),
    typecheck_and_annotate([Form | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {throw, _Context} | T]) ->
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_throw(Stack, Globals, Locals, Form),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [Form = {try_catch_after, _Context} | T]) ->
    {ok, NewLocals, AnnotatedForm} = typecheck_and_annotate_try_catch_after(
        Stack,
        Globals,
        Locals,
        Form
    ),
    typecheck_and_annotate([AnnotatedForm | Acc], Stack, Globals, NewLocals, T);
typecheck_and_annotate(Acc, Stack, Globals, Locals, [H | T]) ->
    typecheck_and_annotate([H | Acc], Stack, Globals, Locals, T);
typecheck_and_annotate(Acc, _Stack, _Globals, Locals, []) ->
    {ok, Locals, lists:reverse(Acc)}.

%% binary_op form helpers

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
    Context1 = Context#{left => AnnotatedLeft, right => AnnotatedRight},
    AnnotatedForm1 = {binary_op, Context1#{locals => Locals}},
    case rufus_type:resolve(Globals, AnnotatedForm1) of
        {ok, TypeForm} ->
            AnnotatedForm2 = {binary_op, Context1#{type => TypeForm}},
            {ok, AnnotatedForm2};
        Error ->
            throw(Error)
    end.

%% call form helpers

%% typecheck_and_annotate_call resolves the return type for a function call and
%% returns a call form annotated with type information.
-spec typecheck_and_annotate_call(rufus_stack(), globals(), locals(), call_form()) ->
    {ok, call_form()} | no_return().
typecheck_and_annotate_call(
    Stack,
    Globals,
    Locals,
    {call, Context = #{args := Args, spec := Spec}}
) ->
    {ok, _NewLocals, AnnotatedArgs} = typecheck_and_annotate([], Stack, Globals, Locals, Args),
    Context1 = Context#{args => AnnotatedArgs},
    Context2 =
        case maps:get(Spec, Locals, undefined) of
            undefined ->
                Context1;
            [_Type] ->
                %% The identifier being invoked refers to a function defined in
                %% the local scope, which means it must be an anonymous
                %% function. We mark the form so that rufus_erlang:forms/1 can
                %% generate the correct Erlang abstract syntax to match the use
                %% case of calling a named function vs. an anonymous function.
                Context1#{kind => anonymous}
        end,
    Form = {call, Context2#{locals => Locals}},
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm = {call, Context2#{type => TypeForm}},
            {ok, AnnotatedForm};
        Error ->
            throw(Error)
    end.

%% cons form helpers

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

%% func form helpers

%% typecheck_and_annotate_func_params resolves and annotates types for each parameter
%% in a function parameter list to ensure they satisfy type constraints.
-spec typecheck_and_annotate_func_params(rufus_stack(), func_form()) ->
    {ok, func_form()} | no_return().
typecheck_and_annotate_func_params(
    Stack,
    Form =
        {func,
            Context = #{
                params := Params,
                exprs := Exprs,
                return_type := ReturnType,
                line := Line
            }}
) ->
    FuncStack = [Form | Stack],
    ParamsStack = [rufus_form:make_func_params(Form) | FuncStack],
    {ok, Locals, AnnotatedParams} = typecheck_and_annotate(
        [],
        ParamsStack,
        #{},
        #{},
        Params
    ),
    ParamTypes = lists:map(fun(ParamForm) -> rufus_form:type(ParamForm) end, AnnotatedParams),
    FuncType = rufus_form:make_type(func, ParamTypes, ReturnType, Line),
    %% Local symbols from the parameter list are captured and stored on the
    %% annotated func form. They're used during the second pass when the
    %% function body is typechecked.
    AnnotatedForm =
        {func, Context#{
            params => AnnotatedParams,
            exprs => Exprs,
            type => FuncType,
            locals => Locals
        }},
    {ok, AnnotatedForm}.

%% typecheck_and_annotate_func adds all parameters to the local scope. It also
%% resolves and annotates types for all expressions in the function body to
%% ensure they satisfy type constraints.
-spec typecheck_and_annotate_func(rufus_stack(), globals(), locals(), func_form()) ->
    {ok, func_form()} | no_return().
typecheck_and_annotate_func(
    Stack,
    Globals,
    Locals1,
    Form =
        {func,
            Context1 = #{
                exprs := Exprs,
                locals := Locals2
            }}
) ->
    %% This version of the function is only called for module-level functions,
    %% which have locals in their context.
    Locals3 = maps:merge(Locals1, Locals2),
    FuncStack = [Form | Stack],
    ExprsStack = [rufus_form:make_func_exprs(Form) | FuncStack],
    {ok, _NewLocals2, AnnotatedExprs} = typecheck_and_annotate(
        [],
        ExprsStack,
        Globals,
        Locals3,
        Exprs
    ),
    Context2 = maps:remove(locals, Context1),
    AnnotatedForm =
        {func, Context2#{
            exprs => AnnotatedExprs
        }},
    ok = typecheck_func_return_type(Globals, AnnotatedForm),
    {ok, AnnotatedForm};
typecheck_and_annotate_func(
    Stack,
    Globals,
    Locals,
    Form =
        {func,
            Context = #{
                params := Params,
                return_type := ReturnType,
                exprs := Exprs,
                line := Line
            }}
) ->
    %% This version of the function is only called for anonymous functions,
    %% which don't have locals in their context.
    FuncStack = [Form | Stack],
    ParamsStack = [rufus_form:make_func_params(Form) | FuncStack],
    {ok, NewLocals1, AnnotatedParams} = typecheck_and_annotate(
        [],
        ParamsStack,
        Globals,
        Locals,
        Params
    ),
    ParamTypes = lists:map(fun(ParamForm) -> rufus_form:type(ParamForm) end, AnnotatedParams),
    FuncType = rufus_form:make_type(func, ParamTypes, ReturnType, Line),

    ExprsStack = [rufus_form:make_func_exprs(Form) | FuncStack],
    {ok, _NewLocals2, AnnotatedExprs} = typecheck_and_annotate(
        [],
        ExprsStack,
        Globals,
        NewLocals1,
        Exprs
    ),
    AnnotatedForm =
        {func, Context#{
            params => AnnotatedParams,
            exprs => AnnotatedExprs,
            type => FuncType
        }},
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
            case ExpectedSpec == ActualSpec orelse rufus_form:type_kind(LastExpr) == throw of
                true ->
                    ok;
                false ->
                    Data = #{
                        globals => Globals,
                        return_type => ReturnType,
                        expr => LastExpr,
                        actual => ActualSpec,
                        expected => ExpectedSpec
                    },
                    throw({error, unmatched_return_type, Data})
            end;
        Error ->
            throw(Error)
    end.

%% identifier form helpers

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
        [TypeForm] ->
            AnnotatedForm2 = {identifier, Context1#{type => TypeForm}},
            io:format("{ok, Locals, AnnotatedForm2} => ~p~n", [{ok, Locals, AnnotatedForm2}]),
            {ok, Locals, AnnotatedForm2}
    end.

%% list_lit form helpers

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

%% match_op form helpers

%% typecheck_and_annotate_match_op ensures that match_op operands have matching
%% types. Unknown identifiers in the left operand are treated as unbound
%% variables and their type information is inferred from the right operand.
%% Return values:
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The match_op form and
%%   its operands are annotated with type information.
%% - `{error, unknown_identifier, Data}` is thrown if the right operand is
%%   unbound.
%% - `{error, unmatched_types, Data}` is thrown when the left and right operand
%%   have differing types.
-spec typecheck_and_annotate_match_op(rufus_stack(), globals(), locals(), match_op_form()) ->
    {ok, locals(), match_op_form()} | no_return().
typecheck_and_annotate_match_op(
    Stack,
    Globals,
    Locals,
    Form = {match_op, Context = #{left := Left, right := Right}}
) ->
    MatchOpStack1 = [Form | Stack],
    RightStack = [rufus_form:make_match_op_right(Form) | MatchOpStack1],
    {ok, NewLocals1, [AnnotatedRight]} = typecheck_and_annotate(
        [],
        RightStack,
        Globals,
        Locals,
        [Right]
    ),
    AnnotatedForm1 = {match_op, Context#{right => AnnotatedRight}},

    MatchOpStack2 = [AnnotatedForm1 | Stack],
    LeftStack = [rufus_form:make_match_op_left(Form) | MatchOpStack2],
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
                {match_op, Context#{
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

%% validate_pattern checks the left hand side of a pattern match_op expression
%% for valid expressions. An `{error, illegal_pattern, Data}` error triple is
%% thrown if an invalid expression is found.
%%
%% TODO(jkakar) Figure out why Dialyzer doesn't like this spec:
%% -spec validate_pattern(globals(), locals(), match_op_form()) -> ok | no_return().
validate_pattern(Globals, Locals, Form = {match_op, _Context}) ->
    Data = #{
        globals => Globals,
        locals => Locals,
        form => Form
    },
    validate_pattern(Data, Form).

%% validate_pattern inspects the left hand operand of a match_op form to ensure
%% that its a valid pattern. A pattern has the same structure as a term but can
%% contain unbound variables. An `{error, illegal_pattern, Data}` error triple
%% is thrown if the left hand operand contains unsupported expressions.
-spec validate_pattern(context(), rufus_form()) -> ok | no_return().
validate_pattern(Data, {match_op, #{left := Left}}) ->
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
validate_pattern(_Data, {cons, _Context}) ->
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

%% throw helpers

typecheck_and_annotate_throw(
    Stack,
    Globals,
    Locals,
    {throw, Context = #{expr := Expr}}
) ->
    {ok, NewLocals, [AnnotatedExpr]} = typecheck_and_annotate([], Stack, Globals, Locals, [Expr]),
    AnnotatedForm1 = {throw, Context#{expr => AnnotatedExpr}},
    case rufus_type:resolve(Stack, Globals, AnnotatedForm1) of
        {ok, TypeForm} ->
            AnnotatedForm2 = {throw, Context#{expr => AnnotatedExpr, type => TypeForm}},
            {ok, NewLocals, AnnotatedForm2};
        Error ->
            throw(Error)
    end.

%% try/catch/after helpers

%% typecheck_and_annotate_catch_clause typechecks and annotates the match
%% expression and body expressions of a catch block. Return values:
%% - `{ok, AnnotatedForm}` if no issues are found. The catch_clause form is
%%   annotated with type information.
%% - An error is thrown is `rufus_type:resolve/3` returns an error.
-spec typecheck_and_annotate_catch_clause(
    rufus_stack(),
    globals(),
    locals(),
    catch_clause_form()
) -> {ok, catch_clause_form()} | no_return().
typecheck_and_annotate_catch_clause(
    Stack,
    Globals,
    Locals,
    Form = {catch_clause, Context = #{match_expr := MatchExpr, exprs := Exprs}}
) ->
    io:format("Locals => ~p~n", [Locals]),
    io:format("MatchExpr => ~p~n", [MatchExpr]),
    CatchClauseStack = [Form | Stack],
    {ok, NewLocals, [AnnotatedMatchExpr]} =
        case MatchExpr of
            undefined ->
                {ok, Locals, [undefined]};
            _ ->
                typecheck_and_annotate(
                    [],
                    CatchClauseStack,
                    Globals,
                    Locals,
                    [MatchExpr]
                )
        end,
    io:format("NewLocals => ~p~n", [NewLocals]),
    io:format("AnnotatedMatchExpr => ~p~n", [AnnotatedMatchExpr]),

    {ok, _, AnnotatedExprs} = typecheck_and_annotate([], Stack, Globals, NewLocals, Exprs),
    AnnotatedForm1 =
        {catch_clause, Context#{
            match_expr => AnnotatedMatchExpr,
            exprs => AnnotatedExprs
        }},
    case rufus_type:resolve(Stack, Globals, AnnotatedForm1) of
        {ok, TypeForm} ->
            AnnotatedForm2 =
                {catch_clause, Context#{
                    match_expr => AnnotatedMatchExpr,
                    exprs => AnnotatedExprs,
                    type => TypeForm
                }},
            io:format("AnnotatedForm2 => ~p~n", [AnnotatedForm2]),
            {ok, AnnotatedForm2};
        Error ->
            throw(Error)
    end.

%% typecheck_and_annotate_try_catch_after ensures that try and catch blocks have
%% a valid sequence of expressions and matching return types. New identifiers in
%% either the try or catch block are not visible in the surrounding scope.
%% Return values:
%% - `{ok, Locals, AnnotatedForm}` if no issues are found. The try_catch_after
%%   form is annotated with type information.
%% - `{error, mismatched_try_catch_return_types, Data}` is thrown if the try and
%%   catch blocks have return values with different types.
-spec typecheck_and_annotate_try_catch_after(
    rufus_stack(),
    globals(),
    locals(),
    try_catch_after_form()
) -> {ok, locals(), try_catch_after_form()} | no_return().
typecheck_and_annotate_try_catch_after(
    Stack,
    Globals,
    Locals,
    {try_catch_after,
        Context = #{
            try_exprs := TryExprs,
            catch_clauses := CatchClauses,
            after_exprs := AfterExprs
        }}
) ->
    {ok, _NewLocals1, AnnotatedTryExprs} = typecheck_and_annotate(
        [],
        Stack,
        Globals,
        Locals,
        TryExprs
    ),

    AnnotatedCatchClauses = lists:map(
        fun(CatchClause) ->
            {ok, _, [AnnotatedCatchClause]} = typecheck_and_annotate(
                [],
                Stack,
                Globals,
                Locals,
                [CatchClause]
            ),
            AnnotatedCatchClause
        end,
        CatchClauses
    ),

    {ok, _NewLocals2, AnnotatedAfterExprs} = typecheck_and_annotate(
        [],
        Stack,
        Globals,
        Locals,
        AfterExprs
    ),

    ok = typecheck_try_catch_return_types(AnnotatedTryExprs, AnnotatedCatchClauses),
    AnnotatedForm1 =
        {try_catch_after, Context#{
            try_exprs => AnnotatedTryExprs,
            catch_clauses => AnnotatedCatchClauses,
            after_exprs => AnnotatedAfterExprs
        }},
    case rufus_type:resolve(Stack, Globals, AnnotatedForm1) of
        {ok, TypeForm} ->
            AnnotatedForm2 =
                {try_catch_after, Context#{
                    try_exprs => AnnotatedTryExprs,
                    catch_clauses => AnnotatedCatchClauses,
                    after_exprs => AnnotatedAfterExprs,
                    type => TypeForm
                }},
            {ok, Locals, AnnotatedForm2};
        Error ->
            throw(Error)
    end.

%% typecheck_try_catch_return_types ensures that the try block and all catch
%% blocks have the same return type.
-spec typecheck_try_catch_return_types(
    rufus_forms(),
    list(catch_clause_form())
) -> ok | no_return().
typecheck_try_catch_return_types(TryExprs, CatchClauses) ->
    LastTryExpr = lists:last(TryExprs),
    TryExprTypeForm = rufus_form:type(LastTryExpr),
    Acc1 =
        case TryExprTypeForm of
            {type, #{kind := throw}} ->
                [];
            _ ->
                [{LastTryExpr, TryExprTypeForm}]
        end,

    CatchClauseTypeForms = lists:map(
        fun(CatchClauseForm) -> {CatchClauseForm, rufus_form:type(CatchClauseForm)} end,
        CatchClauses
    ),
    FormPairs = lists:foldr(
        fun(Element, Acc2) ->
            case Element of
                {_Form, {type, #{kind := throw}}} -> Acc2;
                _ -> [Element | Acc2]
            end
        end,
        Acc1,
        CatchClauseTypeForms
    ),

    validate_try_catch_return_type([], FormPairs).

%% validate_try_catch_return_type iterates over {Form, TypeForm} 2-tuples and
%% returns ok if the types for pairs match, or throws an {error,
%% mismatched_try_catch_return_type, Data} 3-tuple.
%% -spec validate_try_catch_return_type(list(atom), list({rufus_form(), type_form()})) ->
%%     ok | no_return.
validate_try_catch_return_type([Spec], [{_Form, {type, #{spec := Spec}}} | T]) ->
    validate_try_catch_return_type([Spec], T);
validate_try_catch_return_type([], [{_Form, {type, #{spec := Spec}}} | T]) ->
    validate_try_catch_return_type([Spec], T);
validate_try_catch_return_type([ExpectedSpec], [{Form, {type, #{spec := ActualSpec}}} | _T]) ->
    Data = #{
        form => Form,
        actual => ActualSpec,
        expected => ExpectedSpec
    },
    throw({error, mismatched_try_catch_return_type, Data});
validate_try_catch_return_type(_, []) ->
    ok.

%% scope helpers

%% annotate_locals adds a `locals` key to a form context.
-spec annotate_locals(locals(), rufus_form()) -> {ok, rufus_form()}.
annotate_locals(Locals, {FormType, Context}) ->
    {ok, {FormType, Context#{locals => Locals}}}.

%% push_local adds a form to the local scope.
-spec push_local(locals(), rufus_form()) -> {ok, locals()}.
push_local(Locals, {_FormType, #{spec := Spec, type := Type}}) ->
    {ok, Locals#{Spec => [Type]}}.
