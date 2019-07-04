-module(rufus_form).

-include_lib("rufus_type.hrl").

-export([
    line/1,
    source/1,
    spec/1,

    make_module/2,
    make_import/2,
    make_identifier/2,
    make_literal/3,
    make_binary_op/4,
    make_func/5,
    make_arg/3,
    make_inferred_type/2,
    make_type/2
]).

%% Form API

%% line returns the line number from the specified form. A line number is
%% expected with every single form, so lack of one will result in a
%% function_clause exception at runtime.
line({_, #{line := Line}}) ->
    Line.

%% source user_specified, inferred stored for the form.
-spec source({any(), #{source => inferred | speculation | user_specified}}) -> inferred | speculation | user_specified.
source({_, #{source := Source}}) ->
    Source.

%% spec returns the human-readable name for the form.
-spec spec({any(), #{spec => atom()}}) -> atom().
spec({_, #{spec := Spec}}) ->
    Spec.

%% Module form builder API

%% make_module returns a form value for a module declaration.
-spec make_module(atom(), integer()) -> {module, #{spec => atom(), line => integer()}}.
make_module(Spec, Line) ->
    {module, #{spec => Spec, line => Line}}.

-spec make_import(list(), integer()) -> {import, #{spec => list(), line => integer()}}.
make_import(Spec, Line) ->
    {import, #{spec => Spec, line => Line}}.

%% Identifier form builder API

-spec make_identifier(atom(), integer()) -> {identifier, #{spec => atom(), line => integer()}}.
make_identifier(Spec, Line) ->
    {identifier, #{spec => Spec, line => Line}}.

%% Literal form builder API

%% make_literal returns a form for a literal value.
-spec make_literal(bool | float | int | string, atom(), term()) -> bool_lit_form() | float_lit_form() | int_lit_form() | string_lit_form().
make_literal(TypeSpec, Spec, Line) ->
    FormSpec = list_to_atom(unicode:characters_to_list([atom_to_list(TypeSpec), "_lit"])),
    {FormSpec, #{spec => Spec,
                 type => make_inferred_type(TypeSpec, Line),
                 line => Line}}.

%% Binary operation form builder API

%% make_binary_op returns a form for a binary operation.
-spec make_binary_op(atom(), any(), any(), integer()) -> {binary_op, #{op => atom(), left => any(), right => any(), line => integer()}}.
make_binary_op(Op, Left, Right, Line) ->
    {binary_op, #{op => Op, left => Left, right => Right, line => Line}}.

%% Function form builder API

%% make_func returns a form for a function declaration.
-spec make_func(atom(), list(arg_form()), type_form(), list(), integer()) -> {func, #{spec => atom(), args => list(arg_form), return_type => type_form(), exprs => list(), line => integer()}}.
make_func(Spec, Args, ReturnType, Exprs, Line) ->
    {func, #{spec => Spec, args => Args, return_type => ReturnType, exprs => Exprs, line => Line}}.

-spec make_arg(atom(), type_form(), integer()) -> {arg, #{spec => atom(), type => type_form(), line => integer()}}.
make_arg(Spec, Type, Line) ->
    {arg, #{spec => Spec, type => Type, line => Line}}.

%% Type form builder API

%% make_inferred_type creates a type form with 'inferred' as the 'source' value,
%% to indicate that the type has been inferred by the compiler.
-spec make_inferred_type(type_spec(), integer()) -> {type, #{spec => atom(), line => integer()}}.
make_inferred_type(Spec, Line) ->
    make_type(Spec, Line, inferred).

%% make_type returns a type form with 'rufus_text' as the 'source' value, to
%% indicate that the type came from source code.
-spec make_type(atom(), integer()) -> {type, #{spec => atom(), line => integer()}}.
make_type(Spec, Line) ->
    make_type(Spec, Line, rufus_text).

-spec make_type(type_spec(), integer(), inferred | rufus_text) -> type_form().
make_type(Spec, Line, _Source) ->
    {type, #{spec => Spec, line => Line}}.%, source => Source}}.
