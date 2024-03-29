-module(rufus_form).

-include_lib("rufus_type.hrl").

-export([
    %% Form API
    element_type/1,
    has_type/1,
    line/1,
    return_type/1,
    spec/1,
    type/1,
    type_kind/1,
    type_spec/1,
    %% Form builder API
    make_binary_op/4,
    make_binary_op_left/1,
    make_binary_op_right/1,
    make_call/3,
    make_case/3,
    make_case_clause/3,
    make_catch_clause/2,
    make_catch_clause/3,
    make_cons/4,
    make_cons_head/1,
    make_cons_tail/1,
    make_func/4,
    make_func/5,
    make_func_exprs/1,
    make_func_params/1,
    make_identifier/2,
    make_import/2,
    make_literal/3,
    make_literal/4,
    make_match_op/3,
    make_match_op_left/1,
    make_match_op_right/1,
    make_module/2,
    make_param/3,
    make_throw/2,
    make_try_catch_after/4,
    make_type/2,
    make_type/3,
    make_type/4
]).

%% Form API

%% line returns the line number from the specified form. A line number is
%% expected with every single form, so lack of one will result in a
%% function_clause exception at runtime.
-spec line(rufus_form()) -> integer().
line({_, #{line := Line}}) ->
    Line.

%% source returns information about where the type information is from.
-spec return_type(func_form() | type_form()) -> type_form().
return_type({func, #{return_type := ReturnType}}) ->
    ReturnType;
return_type({type, #{kind := func, return_type := ReturnType}}) ->
    ReturnType.

%% spec returns the human-readable name for the form.
-spec spec(rufus_form()) -> atom().
spec({_, #{spec := Spec}}) ->
    Spec.

%% has_type returns true if the form has type information.
-spec has_type(rufus_form()) -> boolean().
has_type({_FormType, #{type := _Type}}) ->
    true;
has_type({identifier, #{spec := Spec, locals := Locals}}) ->
    case maps:get(Spec, Locals, undefined) of
        [{type, _Context}] ->
            true;
        undefined ->
            false
    end;
has_type(_Form) ->
    false.

%% element_type returns the element type for a list type.
-spec element_type(type_form()) -> type_form().
element_type({type, #{kind := list, element_type := ElementType}}) ->
    ElementType;
element_type({_, #{type := {type, #{kind := list, element_type := ElementType}}}}) ->
    ElementType.

%% type returns type information for the form.
-spec type(rufus_form()) -> type_form().
type({_, #{type := Type}}) ->
    Type;
type(Form = {identifier, #{spec := Spec, locals := Locals}}) ->
    case maps:get(Spec, Locals, undefined) of
        [{type, Context}] ->
            {type, Context};
        undefined ->
            {error, unknown_form, #{form => Form}}
    end.

%% type_kind returns the kind for the type of the form, one of func, list, primitive, or throw.
-spec type_kind(rufus_form() | type_form()) -> func | list | primitive | throw.
type_kind({_, #{type := {type, #{kind := Kind}}}}) ->
    Kind;
type_kind({_, #{type := {type, _Context}}}) ->
    primitive;
type_kind({type, #{kind := Kind}}) ->
    Kind;
type_kind({type, _Context}) ->
    primitive.

%% type_spec returns the spec for the type of the form.
-spec type_spec({any(), context()}) -> atom() | error_triple().
type_spec({type, #{spec := TypeSpec}}) ->
    TypeSpec;
type_spec({_, #{type := {type, #{spec := TypeSpec}}}}) ->
    TypeSpec;
type_spec(Form = {identifier, #{spec := Spec, locals := Locals}}) ->
    case maps:get(Spec, Locals, undefined) of
        [{type, #{spec := TypeSpec}}] ->
            TypeSpec;
        undefined ->
            {error, unknown_form, #{form => Form}}
    end.

%% binary_op form builder API

%% make_binary_op returns a binary_op form.
-spec make_binary_op(atom(), rufus_form(), rufus_form(), integer()) ->
    {binary_op, #{op => atom(), left => rufus_form(), right => rufus_form(), line => integer()}}.
make_binary_op(Op, Left, Right, Line) ->
    {binary_op, #{op => Op, left => Left, right => Right, line => Line}}.

%% make_binary_op_left returns a binary_op_left form.
-spec make_binary_op_left(binary_op_form()) -> {binary_op_left, #{line => integer()}}.
make_binary_op_left({binary_op, #{line := Line}}) ->
    {binary_op_left, #{line => Line}}.

%% make_binary_op_right returns a binary_op_right form.
-spec make_binary_op_right(binary_op_form()) -> {binary_op_right, #{line => integer()}}.
make_binary_op_right({binary_op, #{line := Line}}) ->
    {binary_op_right, #{line => Line}}.

%% call form builder API

%% make_call returns a form for a function call.
-spec make_call(atom(), list(), integer()) ->
    {call, #{spec => atom(), args => list(), line => integer()}}.
make_call(Spec, Args, Line) ->
    {call, #{spec => Spec, args => Args, line => Line}}.

%% case form builder API

%% TODO(jkakar): Define a type that correctly defines MatchExpr.
-spec make_case(any(), list(case_clause_form()), integer()) -> case_form().
make_case(MatchExpr, MatchClauses, Line) ->
    {'case', #{match_expr => MatchExpr, clauses => MatchClauses, line => Line}}.

%% make_case_clause returns a form for a match clause in a case block.
%% TODO(jkakar): Define a type that correctly defines MatchExpr.
-spec make_case_clause(any(), rufus_forms(), integer()) -> case_clause_form().
make_case_clause(MatchExpr, Exprs, Line) ->
    {case_clause, #{match_expr => MatchExpr, exprs => Exprs, line => Line}}.

%% cons form builder API

%% make_cons returns a form for a cons expression.
-spec make_cons(type_form(), rufus_form(), list_lit_form(), integer()) -> cons_form().
make_cons(Type, Head, Tail, Line) ->
    {cons, #{type => Type, head => Head, tail => Tail, line => Line}}.

%% make_cons_head returns a head form from a cons expression.
-spec make_cons_head(cons_form()) -> cons_head_form().
make_cons_head({cons, #{line := Line}}) ->
    {cons_head, #{line => Line}}.

%% make_cons_tail returns a tail form from a cons expression.
-spec make_cons_tail(cons_form()) -> cons_tail_form().
make_cons_tail({cons, #{line := Line}}) ->
    {cons_tail, #{line => Line}}.

%% func form builder API

%% make_func returns a form for a function declaration.
-spec make_func(atom(), list(param_form()), type_form(), list(), integer()) ->
    {func, #{
        spec => atom(),
        params => list(param_form),
        return_type => type_form(),
        exprs => list(),
        line => integer()
    }}.
make_func(Spec, Params, ReturnType, Exprs, Line) ->
    {func, #{
        spec => Spec,
        params => Params,
        return_type => ReturnType,
        exprs => Exprs,
        line => Line
    }}.

-spec make_func(list(param_form()), type_form(), list(), integer()) ->
    {func, #{
        params => list(param_form),
        return_type => type_form(),
        exprs => list(),
        line => integer()
    }}.
make_func(Params, ReturnType, Exprs, Line) ->
    {func, #{
        params => Params,
        return_type => ReturnType,
        exprs => Exprs,
        line => Line
    }}.

-spec make_func_exprs(func_form()) -> func_exprs_form().
make_func_exprs({func, #{line := Line}}) ->
    {func_exprs, #{line => Line}}.

%% make_param returns a form for a function parameter declaration.
-spec make_param(atom(), type_form(), integer()) ->
    {param, #{spec => atom(), type => type_form(), line => integer()}}.
make_param(Spec, Type, Line) ->
    {param, #{spec => Spec, type => Type, line => Line}}.

%% make_params returns a form for a function parameter list.
-spec make_func_params(func_form()) -> func_params_form().
make_func_params({func, #{line := Line}}) ->
    {func_params, #{line => Line}}.

%% identifier form builder API

%% make_identifier returns a form for an identifier.
-spec make_identifier(atom(), integer()) -> {identifier, #{spec => atom(), line => integer()}}.
make_identifier(Spec, Line) ->
    {identifier, #{spec => Spec, line => Line}}.

%% import form builder API

%% make_import returns a form for an import statement.
-spec make_import(list(), integer()) -> {import, #{spec => list(), line => integer()}}.
make_import(Spec, Line) ->
    {import, #{spec => Spec, line => Line}}.

%% literal form builder API

%% make_literal returns a form for a literal value.
-spec make_literal(literal(), atom(), term()) -> literal_form().
make_literal(TypeSpec, Spec, Line) ->
    FormSpec = list_to_atom(unicode:characters_to_list([atom_to_list(TypeSpec), "_lit"])),
    {FormSpec, #{
        spec => Spec,
        type => make_type(TypeSpec, Line),
        line => Line
    }}.

-spec make_literal(list, type_form(), list(), term()) -> literal_form().
make_literal(list, Type, Elements, Line) ->
    {list_lit, #{
        elements => Elements,
        type => Type,
        line => Line
    }}.

%% match_op form builder API

%% make_match_op returns a form for a match_op expression.
-spec make_match_op(rufus_form(), rufus_form(), integer()) ->
    {match_op, #{left => rufus_form(), right => rufus_form(), line => integer()}}.
make_match_op(Left, Right, Line) ->
    {match_op, #{left => Left, right => Right, line => Line}}.

%% make_match_op_left returns a left form from a binary_op or match_op expression.
-spec make_match_op_left(match_op_form()) -> match_op_left_form().
make_match_op_left({match_op, #{line := Line}}) ->
    {match_op_left, #{line => Line}}.

%% make_match_op_right returns a right form from a binary_op or match_op expression.
-spec make_match_op_right(match_op_form()) -> match_op_right_form().
make_match_op_right({match_op, #{line := Line}}) ->
    {match_op_right, #{line => Line}}.

%% module form builder API

%% make_module returns a form for a module declaration.
-spec make_module(atom(), integer()) -> {module, #{spec => atom(), line => integer()}}.
make_module(Spec, Line) ->
    {module, #{spec => Spec, line => Line}}.

%% throw form builder API

%% make_throw returns a form for a throw expression.
-spec make_throw(rufus_form(), integer()) -> throw_form().
make_throw(Expr, Line) ->
    {throw, #{expr => Expr, line => Line}}.

%% try/catch/after form builder API

%% make_catch_clause returns a form for a catch clause that matches any error.
%% TODO(jkakar) Figure out why Dialyzer doesn't like this spec:
%% -spec make_catch_clause(any(), integer()) -> catch_clause_form().
make_catch_clause(Exprs, Line) ->
    {FormType, Context} = make_identifier('_', Line),
    UnknownType = make_type(unknown, Line),
    MatchExpr = {FormType, Context#{type => UnknownType}},
    make_catch_clause(MatchExpr, Exprs, Line).

%% make_catch_clause returns a form for a catch clause.
%% TODO(jkakar): Define a type that correctly defines MatchExpr.
-spec make_catch_clause(any(), rufus_forms(), integer()) -> catch_clause_form().
make_catch_clause(MatchExpr, Exprs, Line) ->
    {catch_clause, #{match_expr => MatchExpr, exprs => Exprs, line => Line}}.

%% make_try_catch_after returns a form for a try/catch/after expression.
-spec make_try_catch_after(rufus_forms(), list(catch_clause_form()), rufus_forms(), integer()) ->
    try_catch_after_form().
make_try_catch_after(TryExprs, CatchClauses, AfterExprs, Line) ->
    {try_catch_after, #{
        try_exprs => TryExprs,
        catch_clauses => CatchClauses,
        after_exprs => AfterExprs,
        line => Line
    }}.

%% type form builder API

%% make_type returns a type form with 'rufus_text' as the 'source' value, to
%% indicate that the type came from source code.
-spec make_type(atom(), integer()) ->
    {type, #{spec => atom(), source => type_source(), line => integer()}}.
make_type(Spec, Line) ->
    {type, #{spec => Spec, line => Line}}.

%% make_type returns a list type form with 'rufus_text' as the 'source' value,
%% to indicate that the type came from source code. The type form has a
%% 'collection' key with value 'list', and a 'spec' key with a value that
%% defines the element type.
%% TODO(jkakar) Figure out why Dialyzer doesn't like this spec:
%% -spec make_type(list, {type, context()}, integer()) -> {type, #{kind => list, element_type => type_spec(), spec => atom(), source => type_source(), line => integer()}}.
make_type(list, ElementType, Line) ->
    {type, #{spec := Spec}} = ElementType,
    TypeSpec = list_to_atom(unicode:characters_to_list(["list[", atom_to_list(Spec), "]"])),
    {type, #{
        kind => list,
        element_type => ElementType,
        spec => TypeSpec,
        line => Line
    }};
make_type(throw, ExprType, Line) ->
    ExprTypeSpec = type_spec(ExprType),
    Spec = list_to_atom(unicode:characters_to_list(["throw ", atom_to_list(ExprTypeSpec)])),
    {type, #{
        kind => throw,
        spec => Spec,
        line => Line
    }}.

make_type(func, ParamTypes, ReturnType, Line) ->
    ParamTypeSpecs = lists:map(
        fun(ParamType) ->
            atom_to_list(type_spec(ParamType))
        end,
        ParamTypes
    ),
    ReturnTypeSpec = type_spec(ReturnType),
    Spec = list_to_atom(
        unicode:characters_to_list([
            "func(",
            lists:join(", ", ParamTypeSpecs),
            ") ",
            atom_to_list(ReturnTypeSpec)
        ])
    ),
    {type, #{
        kind => func,
        param_types => ParamTypes,
        return_type => ReturnType,
        spec => Spec,
        line => Line
    }}.
