-module(rufus_form).

-include_lib("rufus_type.hrl").

-export([
    globals/1,
    has_type/1,
    line/1,
    make_binary_op/4,
    make_call/3,
    make_func/5,
    make_identifier/2,
    make_import/2,
    make_inferred_type/2,
    make_literal/3,
    make_match/3,
    make_module/2,
    make_param/3,
    make_type/2,
    return_type/1,
    source/1,
    spec/1,
    type/1,
    type_spec/1
]).

%% Form API

%% line returns the line number from the specified form. A line number is
%% expected with every single form, so lack of one will result in a
%% function_clause exception at runtime.
-spec line(rufus_form()) -> integer().
line({_, #{line := Line}}) ->
    Line.

%% source returns information about where the type information is from.
-spec return_type(func_form()) -> type_form().
return_type({func, #{return_type := ReturnType}}) ->
    ReturnType.

%% source returns information about where the type information is from.
-spec source(type_form()) -> type_source().
source({type, #{source := Source}}) ->
    Source.

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
        {type, _Context} ->
            true;
        undefined ->
            false
    end;
has_type(_Form) ->
    false.

%% type returns type information for the form.
-spec type(rufus_form()) -> context().
type({_, #{type := Type}}) ->
    Type;
type(Form = {identifier, #{spec := Spec, locals := Locals}}) ->
    case maps:get(Spec, Locals, undefined) of
        {type, Context} ->
            {type, Context};
        undefined ->
            {error, unknown_form, #{form => Form}}
    end.

%% globals creates a map of function names to func forms for all top-level
%% functions in RufusForms.
-spec globals(list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(RufusForms) ->
    globals(#{}, RufusForms).

%% type_spec returns the spec for the type of the form.
-spec type_spec({any(), context()}) -> atom() | error_triple().
type_spec({type, #{spec := TypeSpec}}) ->
    TypeSpec;
type_spec({_, #{type := {type, #{spec := TypeSpec}}}}) ->
    TypeSpec;
type_spec(Form = {identifier, #{spec := Spec, locals := Locals}}) ->
    case maps:get(Spec, Locals, undefined) of
        {type, #{spec := TypeSpec}} ->
            TypeSpec;
        undefined ->
            {error, unknown_form, #{form => Form}}
    end.

%% Module form builder API

%% make_module returns a form for a module declaration.
-spec make_module(atom(), integer()) -> {module, #{spec => atom(), line => integer()}}.
make_module(Spec, Line) ->
    {module, #{spec => Spec, line => Line}}.

%% make_import returns a form for an import statement.
-spec make_import(list(), integer()) -> {import, #{spec => list(), line => integer()}}.
make_import(Spec, Line) ->
    {import, #{spec => Spec, line => Line}}.

%% Type form builder API

%% make_inferred_type creates a type form with 'inferred' as the 'source' value,
%% to indicate that the type has been inferred by the compiler.
-spec make_inferred_type(type_spec(), integer()) -> {type, #{spec => atom(), source => type_source(), line => integer()}}.
make_inferred_type(Spec, Line) ->
    make_type(Spec, inferred, Line).

%% make_type returns a type form with 'rufus_text' as the 'source' value, to
%% indicate that the type came from source code.
-spec make_type(atom(), integer()) -> {type, #{spec => atom(), source => type_source(), line => integer()}}.
make_type(Spec, Line) ->
    make_type(Spec, rufus_text, Line).

-spec make_type(type_spec(), type_source(), integer()) -> type_form().
make_type(Spec, Source, Line) ->
    {type, #{spec => Spec, source => Source, line => Line}}.

%% Function form builder API

%% make_func returns a form for a function declaration.
-spec make_func(atom(), list(param_form()), type_form(), list(), integer()) -> {func, #{spec => atom(), params => list(param_form), return_type => type_form(), exprs => list(), line => integer()}}.
make_func(Spec, Params, ReturnType, Exprs, Line) ->
    {func, #{spec => Spec, params => Params, return_type => ReturnType, exprs => Exprs, line => Line}}.

%% make_param returns a form for a function parameter declaration.
-spec make_param(atom(), type_form(), integer()) -> {param, #{spec => atom(), type => type_form(), line => integer()}}.
make_param(Spec, Type, Line) ->
    {param, #{spec => Spec, type => Type, line => Line}}.

%% make_call returns a form a function call.
-spec make_call(atom(), list(), integer()) -> {call, #{spec => atom(), args => list(), line => integer()}}.
make_call(Spec, Args, Line) ->
    {call, #{spec => Spec, args => Args, line => Line}}.

%% Identifier form builder API

%% make_identifier returns a form for an identifier.
-spec make_identifier(atom(), integer()) -> {identifier, #{spec => atom(), line => integer()}}.
make_identifier(Spec, Line) ->
    {identifier, #{spec => Spec, line => Line}}.

%% Literal form builder API

%% make_literal returns a form for a literal value.
-spec make_literal(literal(), atom(), term()) -> literal_form().
make_literal(TypeSpec, Spec, Line) ->
    FormSpec = list_to_atom(unicode:characters_to_list([atom_to_list(TypeSpec), "_lit"])),
    {FormSpec, #{spec => Spec,
                 type => make_inferred_type(TypeSpec, Line),
                 line => Line}}.

%% binary_op form builder API

%% make_binary_op returns a form for a binary operation.
-spec make_binary_op(atom(), rufus_form(), rufus_form(), integer()) -> {binary_op, #{op => atom(), left => rufus_form(), right => rufus_form(), line => integer()}}.
make_binary_op(Op, Left, Right, Line) ->
    {binary_op, #{op => Op, left => Left, right => Right, line => Line}}.

%% match form builder API

%% make_match returns a form for a match expression.
-spec make_match(rufus_form(), rufus_form(), integer()) -> {match, #{left => rufus_form(), right => rufus_form(), line => integer()}}.
make_match(Left, Right, Line) ->
    {match, #{left => Left, right => Right, line => Line}}.

%% Private API

-spec globals(map(), list(rufus_form())) -> {ok, #{atom() => list(rufus_form())}}.
globals(Acc, [Form = {func, #{spec := Spec}}|T]) ->
    Forms = maps:get(Spec, Acc, []),
    globals(Acc#{Spec => Forms ++ [Form]}, T);
globals(Acc, [_H|T]) ->
    globals(Acc, T);
globals(Acc, []) ->
    {ok, Acc}.
