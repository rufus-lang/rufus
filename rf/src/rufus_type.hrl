%% Result types

-type ok_tuple() :: {ok, any()}.
-type error_tuple() :: {error, any()}.

%% Rufus source text

-type rufus_text() :: string().

%% State storage

-type locals() :: map().

%% Types

-type type_spec() :: atom().
-type type_form() :: {type, map()}.

%% Modules

-type module_form() :: {module, map()}.

%% Scalar literals

-type bool_lit_form() :: {bool_lit, map()}.
-type float_lit_form() :: {float_lit, map()}.
-type int_lit_form() :: {int_lit, map()}.
-type string_lit_form() :: {string_lit, map()}.

%% Expressions

-type func_form() :: {func, map()}.
-type arg_form() :: {arg, map()}.
-type identifier_form() :: {identifier, map()}.
-type binary_op_form() :: {binary_op, map()}.

%% Rufus forms

-type rufus_form() ::
        type_form()
      | bool_lit_form()
      | float_lit_form()
      | int_lit_form()
      | string_lit_form()
      | module_form()
      | func_form()
      | arg_form()
      | identifier_form()
      | binary_op_form().

%% Errors

-type unknown_variable_error() :: unknown_variable.
-type unmatched_return_type_error() :: unmatched_return_type.
-type unmatched_operand_type_error() :: unmatched_operand_type.
-type unsupported_operand_type_error() :: unsupported_operand_type.
-type rufus_error() ::
        unknown_variable_error()
      | unmatched_return_type_error()
      | unmatched_operand_type_error()
      | unsupported_operand_type_error().

%% Erlang forms

-type erlang_form() :: any().
