%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2015 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc Lexer
%%% @end

Definitions.

INT = [0-9]
FLOAT = [0-9]+\.[0-9]+
LETTER = [a-zA-Z_\.0-9]
STRING = "[\-\$\.\^\*\!\sa-zA-Z_0-9]+"
WHITESPACE = [\s\t\n\r]
ARITHM_OP = [+-]
EQUAL_OP = (:|!:|~)
COMP_OP = (<|<:|>:|>)
TIME=[0-2][0-9]:[0-5][0-9]:[0-5][0-9]
DATE=[0-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]

Rules.

{WHITESPACE}+ : skip_token.
in : {token, {in_op, TokenLine, list_to_binary(TokenChars)}}.
not : {token, {not_op, TokenLine, list_to_binary(TokenChars)}}.
exists : {token, {exists_op, TokenLine, list_to_binary(TokenChars)}}.
now : {token, {timestamp, TokenLine, erlang:timestamp()}}.
now\s*{ARITHM_OP}\s*{INT}+\s*[hms] : {token, {timestamp, TokenLine, split_interval(TokenChars)}}.
{ARITHM_OP}?{INT}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{ARITHM_OP}?{INT}+\.{INT}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{LETTER}+ : {token, {field, TokenLine, list_to_binary(TokenChars)}}.
{STRING} : {token, {string, TokenLine, list_to_binary(string:substr(TokenChars, 2, length(TokenChars) - 2))}}.
{DATE}T{TIME}Z : {token, {timestamp, TokenLine, mongoql_utils:iso8601totimestamp(TokenChars)}}.
{LETTER}+\-desc : {token, {order_descending, TokenLine, list_to_binary(string:substr(TokenChars, 1, length(TokenChars) - length("-desc")))}}.
{LETTER}+\-asc : {token, {order_ascending, TokenLine, list_to_binary(string:substr(TokenChars, 1, length(TokenChars) - length("-asc")))}}.
{EQUAL_OP} : {token, {equal_op, TokenLine, list_to_binary(TokenChars)}}.
{COMP_OP} : {token, {comp_op, TokenLine, list_to_binary(TokenChars)}}.
\[ : {token, {square_bracket_open_op, TokenLine, list_to_binary(TokenChars)}}.
\] : {token, {square_bracket_close_op, TokenLine, list_to_binary(TokenChars)}}.
[.]+ : {error, syntax}.

Erlang code.

split_interval(String) ->
  Now = erlang:timestamp(),
  [Split] = string:tokens(mongoql_utils:remove_spaces(String), "now"),
  {Num, Time} = string:to_integer(Split),
  mongoql_utils:now_less_interval(Now, Num, Time).
