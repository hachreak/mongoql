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
EQUAL_OP = (:)
COMP_OP = (<|<:|>:|>|!:)

Rules.

{WHITESPACE}+ : skip_token.
{LETTER}+ : {token, {field, TokenLine, list_to_binary(TokenChars)}}.
{STRING} : {token, {string, TokenLine, list_to_binary(string:substr(TokenChars, 2, string:len(TokenChars) - 2))}}.
{ARITHM_OP}?{INT}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{ARITHM_OP}?{INT}+\.{INT}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.
\-{LETTER}+ : {token, {order_descending, TokenLine, list_to_binary(lists:delete($-, TokenChars))}}.
\+{LETTER}+ : {token, {order_ascending, TokenLine, list_to_binary(lists:delete($+, TokenChars))}}.
{EQUAL_OP} : {token, {equal_op, list_to_binary(TokenChars)}}.
{COMP_OP} : {token, {comp_op, list_to_binary(TokenChars)}}.
[.]+ : {error, syntax}.

Erlang code.
