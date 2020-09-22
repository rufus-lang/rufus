%% Context container

-type context() :: #{atom() := any()}.

%% Result types

-type ok_tuple() :: {ok, any()}.
-type error_tuple() :: {error, any()}.
-type error_triple() :: {error, any(), context()}.

%% Rufus source text

-type rufus_text() :: string().

%% State storage

-type globals() :: context().
-type locals() :: context().

%% Types

-type collection_type_spec() :: list.
-type type_spec() :: atom().
-type type_form() :: {type, context()}.
-type type_source() :: inferred | rufus_text.

%% Modules

-type module_form() :: {module, context()}.

%% Scalar literals

-type atom_lit_form() :: {atom_lit, context()}.
-type bool_lit_form() :: {bool_lit, context()}.
-type float_lit_form() :: {float_lit, context()}.
-type int_lit_form() :: {int_lit, context()}.
-type string_lit_form() :: {string_lit, context()}.
-type list_lit_form() :: {list_lit, context()}.
-type cons_form() :: {cons, context()}.

-type literal_form() ::
        atom_lit_form()
      | bool_lit_form()
      | float_lit_form()
      | int_lit_form()
      | string_lit_form()
      | list_lit_form()
      | cons_form().

-type literal() ::
        atom
      | bool
      | float
      | int
      | string.

%% Operators

-type arithmetic_operator() :: '+' | '-' | '*' | '/' | '%'.
-type boolean_operator() :: 'and' | 'or'.
-type comparison_operator() :: '==' | '!=' | '<' | '<=' | '>' | '>='.

%% Expressions

-type func_form() :: {func, context()}.
-type param_form() :: {param, context()}.
-type identifier_form() :: {identifier, context()}.
-type binary_op_form() :: {binary_op, context()}.
-type match_form() :: {match, context()}.
-type call_form() :: {call, context()}.

%% Rufus forms

%% rufus_form represents a node in the parse tree.
-type rufus_form() ::
        atom_lit_form()
      | bool_lit_form()
      | float_lit_form()
      | int_lit_form()
      | string_lit_form()
      | list_lit_form()
      | cons_form()
      | module_form()
      | func_form()
      | param_form()
      | identifier_form()
      | type_form()
      | binary_op_form()
      | match_form()
      | call_form().
%% rufus_forms is a list of rufus_form instances and typically represents an
%% entire module.
-type rufus_forms() :: list(rufus_form()).
%% rufus_stack is a list of rufus_form instances that represent the path from
%% the current node up to the top-most root node.
-type rufus_stack() :: list(rufus_form()).

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

-type erlang3_form() :: {_, _, _}.
-type erlang4_form() :: {_, _, _, _}.
-type erlang5_form() :: {_, _, _, _, _}.
-type erlang_form() ::
        erlang3_form()
      | erlang4_form()
      | erlang5_form().

-type export_attribute_erlang_form() :: {attribute, integer(), export, list()}.
