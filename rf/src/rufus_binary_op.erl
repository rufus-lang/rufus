%% rufus_binary_op enforces the invariants that a binary operation may only be
%% performed exclusively with ints or exclusively with floats, not both at the
%% same time. No other types are supported with binary operators. `binary_op`
%% forms are annotated with their type, which is inferred if necessary.
-module(rufus_binary_op).

-include_lib("rufus_type.hrl").

%% API exports

-export([typecheck_and_annotate/1]).

%% API

%% typecheck_and_annotate iterates over RufusForms and typechecks binary
%% operations to ensure that the operands are exclusively ints or exclusively
%% floats. Inferred type information is added to every `binary_op` form.
%% Iteration stops at the first error. Returns values:
%% - `{ok, AnnotatedForms}` if no issues are found with every `binary_op` form
%%   having an inferred type annotation.
%% - `{error, unmatched_operand_type, Form}` if an `int` operand is mixed with a
%%   `float` operand. `Form` contains the illegal operands.
%% - `{error, unsupported_operand_type, Form}` if a type other than an int is
%%   used as an operand. `Form` contains the illegal operands.
-spec typecheck_and_annotate(list(rufus_form())) -> {ok, list(rufus_form())} | error_triple().
typecheck_and_annotate(RufusForms) ->
    {ok, Globals} = rufus_expr:globals(RufusForms),
    try
        typecheck_and_annotate([], Globals, RufusForms)
    catch
        {error, Code, Data} -> {error, Code, Data}
    end.

%% Private API

-spec typecheck_and_annotate(list(rufus_form()), #{atom() => rufus_form()}, list(rufus_form())) -> {ok, list(rufus_form())} | no_return().
typecheck_and_annotate(Acc, Globals, [{func_decl, Context = #{exprs := Exprs}}|T]) ->
    {ok, AnnotatedExprs} = typecheck_and_annotate([], Globals, Exprs),
    AnnotatedForm = {func_decl, Context#{exprs => AnnotatedExprs}},
    typecheck_and_annotate([AnnotatedForm|Acc], Globals, T);
typecheck_and_annotate(Acc, Globals, [Form = {binary_op, #{left := Left, right := Right}}|T]) ->
    {ok, [AnnotatedLeft]} = typecheck_and_annotate([], Globals, [Left]),
    {ok, [AnnotatedRight]} = typecheck_and_annotate([], Globals, [Right]),
    case rufus_type:resolve(Globals, Form) of
        {ok, TypeForm} ->
            AnnotatedForm1 = rufus_form:annotate(Form, type, TypeForm),
            AnnotatedForm2 = rufus_form:annotate(AnnotatedForm1, left, AnnotatedLeft),
            AnnotatedForm3 = rufus_form:annotate(AnnotatedForm2, right, AnnotatedRight),
            typecheck_and_annotate([AnnotatedForm3|Acc], Globals, T);
        {error, Code, Data} ->
            throw({error, Code, Data})
    end;
typecheck_and_annotate(Acc, Globals, [H|T]) ->
    typecheck_and_annotate([H|Acc], Globals, T);
typecheck_and_annotate(Acc, _Globals, []) ->
    {ok, lists:reverse(Acc)}.
