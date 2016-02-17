%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2016 Leonardo Rossi
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
%%% @doc MongoQL Utils
%%% @end

-module('mongoql_utils').

%% API exports
-export([iso8601totimestamp/1, datetime2timestamp/1]).


iso8601totimestamp(DateTimeString) ->
  DateTime = lists:map(fun(V) -> list_to_integer(V) end,
                       string:tokens(DateTimeString, ":T-Z")),
  datetime2timestamp(DateTime).

datetime2timestamp([Y, M, D, H, Min, S]) ->
  Seconds = calendar:datetime_to_gregorian_seconds(
              {{Y, M, D}, {H, Min, S}}) - 62167219200,
  {Seconds div 1000000, Seconds rem 1000000, 0}.
