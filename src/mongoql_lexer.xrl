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
LETTER = [a-zA-Z_\.]
STRING = "(\\\^.|\\.|[^\"])*"
WHITESPACE = [\s\t\n\r]
ARITHM_OP = [+-]
EQUAL_OP = (:|!:)
COMP_OP = (<|<:|>:|>)
TIME=[0-2][0-9]:[0-5][0-9]:[0-5][0-9]
DATE=[0-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]

Rules.

{WHITESPACE}+ : skip_token.
{LETTER}+ : {token, {field, TokenLine, list_to_binary(TokenChars)}}.
{STRING} : {token, {string, TokenLine, list_to_binary(string:substr(TokenChars, 2, length(TokenChars) - 2))}}.
{DATE}T{TIME}Z : {token, {timestamp, TokenLine, iso8601totimestamp(TokenChars)}}.
{ARITHM_OP}?{INT}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{ARITHM_OP}?{INT}+\.{INT}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{LETTER}+\-desc : {token, {order_descending, TokenLine, list_to_binary(string:substr(TokenChars, 1, length(TokenChars) - length("-desc")))}}.
{LETTER}+\-asc : {token, {order_ascending, TokenLine, list_to_binary(string:substr(TokenChars, 1, length(TokenChars) - length("-asc")))}}.
{EQUAL_OP} : {token, {equal_op, list_to_binary(TokenChars)}}.
{COMP_OP} : {token, {comp_op, list_to_binary(TokenChars)}}.
[.]+ : {error, syntax}.

Erlang code.

iso8601totimestamp(DateTimeString) ->
  DateTime = lists:map(fun(V) -> list_to_integer(V) end,
                       string:tokens(DateTimeString, ":T-Z")),
  datetime2timestamp(DateTime).

datetime2timestamp([Y, M, D, H, Min, S]) ->
  Seconds = calendar:datetime_to_gregorian_seconds(
              {{Y, M, D}, {H, Min, S}}) - 62167219200,
  {Seconds div 1000000, Seconds rem 1000000, 0}.
