%% rufus_tokenize inserts expression terminators into a stream of tokens
%% produced by rufus_raw_tokenize, based on the set of rules defined in the
%% _Semicolons_ section of the language spec:
%%
%% 1. When the input is broken into tokens, a semicolon is automatically
%%    inserted into the token stream immediately after a line's final token if
%%    that token is:
%%    - An identifier.
%%    - An `atom`, `bool`, `float`, `int` or `string` literal.
%%    - One of the punctuation `)` or `}`.
%% 2. To allow complex expressions to occupy a single line, a semicolon may be
%%    omitted before a closing `)` or `}`.
-module(rufus_tokenize).

-include_lib("rufus_type.hrl").

%% API exports

-export([string/1]).

%% API

%% string tokenizes RufusText and inserts semicolon expression terminators based
%% on a set of rules. Return values:
%% - `{ok, Tokens}` with `Tokens` as a list of tokens, if tokenization is
%%   successful.
%% - `{error, Reason}` if an error occurs.
-spec string(rufus_text()) -> ok_tuple() | error_tuple().
string(RufusText) ->
    case rufus_raw_tokenize:string(RufusText) of
        {ok, Tokens, _Lines} ->
            insert_semicolons(Tokens);
        {error, Reason, _LineNumber} ->
            {error, Reason}
    end.

%% Private API

make_semicolon_token(TokenLine) ->
    {';', TokenLine}.

terminate(Acc=[{';', _TokenLine1}|_T], _TokenLine2) ->
    Acc;
terminate(Acc, TokenLine) ->
    [make_semicolon_token(TokenLine)|Acc].

%% insert_semicolons inserts `;` tokens after some `eol` tokens to terminate
%% expressions. All `eol` tokens are discarded in the resulting list of tokens.
-spec insert_semicolons(list(tuple())) -> {ok, list(tuple())}.
insert_semicolons(Tokens) ->
    LastToken = undefined,
    SemicolonTerminatedTokens = insert_semicolons([], LastToken, Tokens),
    {ok, SemicolonTerminatedTokens}.

%% insert_semicolons inserts a semicolon when the following tokens are the last
%% on a line or the last in the source text. The `eol` token is always discarded.
insert_semicolons(Acc, {identifier, TokenLine, _TokenChars}, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(terminate(Acc, TokenLine), Token, T);
insert_semicolons(Acc, {identifier, TokenLine, _TokenChars}, []) ->
    insert_semicolons(terminate(Acc, TokenLine), undefined, []);
insert_semicolons(Acc, {atom_lit, TokenLine, _TokenChars}, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(terminate(Acc, TokenLine), Token, T);
insert_semicolons(Acc, {atom_lit, TokenLine, _TokenChars}, []) ->
    insert_semicolons(terminate(Acc, TokenLine), undefined, []);
insert_semicolons(Acc, {bool_lit, TokenLine, _TokenChars}, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(terminate(Acc, TokenLine), Token, T);
insert_semicolons(Acc, {bool_lit, TokenLine, _TokenChars}, []) ->
    insert_semicolons(terminate(Acc, TokenLine), undefined, []);
insert_semicolons(Acc, {float_lit, TokenLine, _TokenChars}, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(terminate(Acc, TokenLine), Token, T);
insert_semicolons(Acc, {float_lit, TokenLine, _TokenChars}, []) ->
    insert_semicolons(terminate(Acc, TokenLine), undefined, []);
insert_semicolons(Acc, {int_lit, TokenLine, _TokenChars}, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(terminate(Acc, TokenLine), Token, T);
insert_semicolons(Acc, {int_lit, TokenLine, _TokenChars}, []) ->
    insert_semicolons(terminate(Acc, TokenLine), undefined, []);
insert_semicolons(Acc, {string_lit, TokenLine, _TokenChars}, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(terminate(Acc, TokenLine), Token, T);
insert_semicolons(Acc, {string_lit, TokenLine, _TokenChars}, []) ->
    insert_semicolons(terminate(Acc, TokenLine), undefined, []);
insert_semicolons(Acc, {')', TokenLine}, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(terminate(Acc, TokenLine), Token, T);
insert_semicolons(Acc, {')', TokenLine}, []) ->
    insert_semicolons(terminate(Acc, TokenLine), undefined, []);
insert_semicolons(Acc, {'}', TokenLine}, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(terminate(Acc, TokenLine), Token, T);
insert_semicolons(Acc, {'}', TokenLine}, []) ->
    insert_semicolons(terminate(Acc, TokenLine), undefined, []);
%% insert_semicolons discards `eol` tokens.
insert_semicolons(Acc, _LastToken, [Token = {eol, _TokenLine}|T]) ->
    insert_semicolons(Acc, Token, T);
%% insert_semicolons keeps all other tokens.
insert_semicolons(Acc, _LastToken, [Token|T]) ->
    insert_semicolons([Token|Acc], Token, T);
%% insert_semicolons terminates when all tokens have been processed.
insert_semicolons(Acc, _LastToken, []) ->
    lists:reverse(Acc).
