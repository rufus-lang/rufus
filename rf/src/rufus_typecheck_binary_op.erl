%% rufus_typecheck_binary_op enforces the invariant that a binary operation may
%% only be performed exclusively with ints or exclusively with floats, not both
%% at the same time. No other types are supported with binary operators.
-module(rufus_typecheck_binary_op).

-include_lib("rufus_type.hrl").

%% API exports

-export([forms/1]).

%% API

%% forms iterates over RufusForms and typechecks binary operations to ensure
%% that the operands are exclusively ints or exclusively floats. Iteration stops
%% at the first error. Returns values:
%% - `{ok, AnnotatedForms}` if no issues are found with all `binary_op` forms
%%   have type annotations.
%% - `{error, unmatched_operand_type, Data}` if an `int` operand is mixed with a
%%   `float` operand. `Data` contains `left` and `right` atom keys pointing to
%%   the illegal operands.
%% - `{error, unsupported_operand_type, Data}` if a type other than an int is
%%   used as an operand. `Data` contains `left` and `right` atom keys pointing
%%   to the illegal operands.
-spec forms(list(rufus_form())) -> {ok, list(rufus_form())}.
forms(RufusForms) ->
    forms([], RufusForms).

%% Private API

forms(Acc, [{func, Context=#{exprs := Exprs}}|T]) ->
    case forms([], Exprs) of
        {ok, AnnotatedExprs} ->
            AnnotatedForm = {func, Context#{exprs => AnnotatedExprs}},
            forms([AnnotatedForm|Acc], T);
        Error ->
            Error
    end;
forms(Acc, [Form={binary_op, _Context}|T]) ->
    case typecheck_and_annotate(Form) of
        {ok, AnnotatedForm} ->
            forms([AnnotatedForm|Acc], T);
        Error ->
            Error
    end;
forms(Acc, [H|T]) ->
    forms([H|Acc], T);
forms(Acc, []) ->
    {ok, lists:reverse(Acc)}.

typecheck_and_annotate({binary_op, Context = #{left := [Left], right := [Right]}}) ->
    {ok, AnnotatedLeft} = typecheck_and_annotate(Left),
    {ok, AnnotatedRight} = typecheck_and_annotate(Right),
    case infer_binary_op_type({binary_op, Context#{left => AnnotatedLeft, right => AnnotatedRight}}) of
        {ok, AnnotatedForm = {binary_op, _}} ->
            {ok, AnnotatedForm};
        Error ->
            Error
    end;
typecheck_and_annotate(Form = {_, #{type := _}}) ->
    {ok, Form};
typecheck_and_annotate(Form) ->
    erlang:error({unhandled_form, Form}).

infer_binary_op_type(Form = {binary_op, Context = #{left := Left, right := Right}}) ->
    LeftType = rufus_form:type(Left),
    LeftTypeSpec = rufus_form:spec(LeftType),
    RightType = rufus_form:type(Right),
    RightTypeSpec = rufus_form:spec(RightType),
    case supported_type(LeftTypeSpec) and supported_type(RightTypeSpec) of
        true ->
            case supported_type_pair(LeftTypeSpec, RightTypeSpec) of
                true ->
                    {ok, {binary_op, Context#{type => LeftType}}};
                false ->
                    {error, unmatched_operand_type, Form}
            end;
        false ->
            {error, unsupported_operand_type, Form}
    end.

supported_type(float) -> true;
supported_type(int) -> true;
supported_type(_) -> false.

supported_type_pair(float, float) -> true;
supported_type_pair(int, int) -> true;
supported_type_pair(_, _) -> false.
