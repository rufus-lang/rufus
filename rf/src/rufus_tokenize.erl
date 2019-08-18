%% rufus_tokenize scans Rufus source code and generates tokens for the
%% rufus_parse module to consume. The rufus_scan module is used as a low-level
%% scanner. The tokenizer processes scanned tokens and inserts expression
%% terminators based on a set of rules:
%%
%% 1. When the input is broken into tokens, a semicolon is automatically
%%    inserted into the token stream immediately after a line's final token if
%%    that token is:
%%    - An identifier.
%%    - A `bool`, `float`, `int` or `string` literal.
%%    - One of the punctuation `)`, `]`, or `}`.
%% 2. To allow complex expressionss to occupy a single line, a semicolon may be
%%    omitted before a closing `)` or `}`.
-module(rufus_tokenize).

%% API exports

-export([string/1]).

%% API

%% string tokenizes RufusText and inserts expression terminators based on a set
%% of rules. Return values:
%% - `{ok, Tokens}` with `Tokens` as a list of tokens, if tokenization is
%%   successful.
%% - `{error, Reason}` if an error occurs.
string(RufusText) ->
    case rufus_scan:string(RufusText) of
        {ok, Tokens, _Lines} ->
            insert_semicolons(Tokens);
        {error, Reason, _LineNumber} ->
            {error, Reason}
    end.

%% Private API

%% insert_semicolons inserts `;` tokens after some `eol` tokens to terminate
%% expressions. All `eol` tokens are discarded in the resulting list of tokens.
insert_semicolons(Tokens) ->
    case insert_semicolons([], undefined, Tokens) of
        {error, Reason} ->
            {error, Reason};
        ExprTerminatedTokens ->
            {ok, ExprTerminatedTokens}
    end.

insert_semicolons(Acc, PrevToken={identifier, _TokenLine, _TokenChars}, [Token={eol, _TokenLine}|T]) ->
    insert_semicolons(insert_semicolon(Acc, PrevToken, Token), Token, T);
insert_semicolons(Acc, PrevToken={bool_lit, _TokenLine, _TokenChars}, [Token={eol, _TokenLine}|T]) ->
    insert_semicolons(insert_semicolon(Acc, PrevToken, Token), Token, T);
insert_semicolons(Acc, PrevToken={float_lit, _TokenLine, _TokenChars}, [Token={eol, _TokenLine}|T]) ->
    insert_semicolons(insert_semicolon(Acc, PrevToken, Token), Token, T);
insert_semicolons(Acc, PrevToken={int_lit, _TokenLine, _TokenChars}, [Token={eol, _TokenLine}|T]) ->
    insert_semicolons(insert_semicolon(Acc, PrevToken, Token), Token, T);
insert_semicolons(Acc, PrevToken={string_lit, _TokenLine, _TokenChars}, [Token={eol, _TokenLine}|T]) ->
    insert_semicolons(insert_semicolon(Acc, PrevToken, Token), Token, T);
insert_semicolons(Acc, PrevToken={')', _TokenLine, _TokenChars}, [Token={eol, _TokenLine}|T]) ->
    insert_semicolons(insert_semicolon(Acc, PrevToken, Token), Token, T);
insert_semicolons(Acc, PrevToken={'}', _TokenLine, _TokenChars}, [Token={eol, _TokenLine}|T]) ->
    insert_semicolons(insert_semicolon(Acc, PrevToken, Token), Token, T);
insert_semicolons(Acc, _PrevToken, [Token|T]) ->
    insert_semicolons([Token|Acc], Token, T);
insert_semicolons(Acc, _PrevToken, []) ->
    lists:reverse(Acc).

make_semicolon_token(TokenLine) ->
    {';', TokenLine}.

insert_semicolon(Acc, _PrevToken={_Type, TokenLine, _TokenChars1}, Token) ->
    Acc1 = [make_semicolon_token(TokenLine)|Acc],
    [Token|Acc1].
