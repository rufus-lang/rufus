Definitions.

Newline       = \n
UnicodeLetter = [A-Za-z]
Digit         = [0-9]
Letter        = ({UnicodeLetter}|"_")
Identifier    = {Letter}({Letter}|{Digit})*
Whitespace    = [\s\t]

Rules.

package       : {token, {package, TokenLine, TokenChars}}.
{Identifier}  : {token, {identifier, TokenLine, TokenChars}}.
{Whitespace}+ : skip_token.

Erlang code.

%% Definitions.

%% Newline       = \n
%% UnicodeLetter = [A-Za-z]
%% Digit         = [0-9]
%% Letter        = ({UnicodeLetter}|"_")
%% Identifier    = {Letter}({Letter}|{Digit})*
%% WS            = ([\000-\s]|%.*)

%% Rules.

%% (package)\s+{Identifier} : {token, {package, TokenLine, TokenChars}}.
%% \.{WS}                 : {end_token, {dot, TokenLine}}.
%% {WS}+                  : skip_token.

%% Erlang code.

%% strip(TokenChars,TokenLen) ->
%%     lists:sublist(TokenChars, 2, TokenLen - 2).

%% in     : {token,{set,TokenLine,list_to_atom(TokenChars)}}.
%% or     : {token,{union,TokenLine,list_to_atom(TokenChars)}}.
%% and    : {token,{intersection,TokenLine,list_to_atom(TokenChars)}}.
%% {C}    : {token,{comparator,TokenLine,list_to_atom(TokenChars)}}.
%% '{L}+' : S = strip(TokenChars,TokenLen),
%%          {token,{string,TokenLine,S}}.
%% {L}+   : {token,{var,TokenLine,list_to_atom(TokenChars)}}.
%% {D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
%% [(),]  : {token,{list_to_atom(TokenChars),TokenLine}}.
%% {WS}+  : skip_token.
