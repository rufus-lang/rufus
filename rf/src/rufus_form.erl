-module(rufus_form).

-include_lib("rufus_type.hrl").

-export([
    line/1,
    make_literal/3,
    make_inferred_type/2,
    make_user_specified_type/2,
    source/1,
    spec/1
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

%% Literal form builder API

%% make_literal returns a form for a literal value.
-spec make_literal(bool | float | int | string, atom(), term()) -> bool_lit_form() | float_lit_form() | int_lit_form() | string_lit_form().
make_literal(TypeSpec, Spec, Line) ->
    FormType = list_to_atom(unicode:characters_to_list([atom_to_list(TypeSpec), "_lit"])),
    {FormType, #{line => Line,
                 spec => Spec,
                 type => {type, #{line => Line,
                                  spec => TypeSpec}}}}.

%% Type form builder API

-spec make_user_specified_type(atom(), integer()) -> {type, #{line => integer(), spec => atom()}}.
make_user_specified_type(Spec, Line) ->
    make_type(Spec, Line, user_specified).

-spec make_inferred_type(type_spec(), integer()) -> {type, #{line => integer(), spec => atom()}}.
make_inferred_type(Spec, Line) ->
    make_type(Spec, Line, inferred).

-spec make_type(type_spec(), integer(), inferred | user_specified) -> type_form().
make_type(Spec, Line, _Source) ->
    {type, #{spec => Spec, line => Line}}.%, source => Source}}.
