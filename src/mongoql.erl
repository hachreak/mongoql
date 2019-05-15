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
%%% @doc MongoQL API
%%% @end

-module('mongoql').

%% API exports
-export([
  agg/1,
  agg/2,
  build/2,
  parse/1,
  parse/2
]).

-export_type([query/0]).

%%%_ * Types -----------------------------------------------------------

-type query() :: list() | binary().

%%====================================================================
%% API functions
%%====================================================================

% @doc Build a query with predefined parameters.
% build("latitude:12 longitude:23", [{'temperature', 12}]).
% It's the same to parse:
% parse("latitude:12 longitude:23 temperature:12").
% Or
% parse("latitude:12 longitude:23 temperature:{{temp}}", [{temp, 12}]).
% @end
-spec build(query(), list()) -> list().
build(Query, Args) when is_list(Query) ->
  try
    Query2 = lists:foldl(
      fun({Key, Value}, Acc) when is_list(Value) or is_binary(Value) ->
        Acc ++ sf:format(" {{key}}:\"{{{{key}}}}\"", [{key, Key}], [string]);
         ({Key, _Value}, Acc) ->
        Acc ++ sf:format(" {{key}}:{{{{key}}}}", [{key, Key}], [string])
      end, "", Args),
    mongoql:parse(Query ++ Query2, Args)
  catch
    _:_ -> {error, invalid_query}
  end;
build(Query, Args) ->
  build(binary_to_list(Query), Args).

parse(Query, Args) -> parse(sf:format(Query, Args)).

-spec parse(query()) -> {ok, list()} | {error, unknow_input}.
parse(Query) when is_binary(Query) ->
  parse(binary_to_list(Query));
parse(Query) when is_list(Query) ->
  {ok, Tokens, _} = mongoql_lexer:string(Query),
  mongoql_parser:parse(Tokens);
parse(_Query) ->
  {error, unknow_input}.

agg(Query) when is_binary(Query) ->
  agg(binary_to_list(Query));
agg(Query) when is_list(Query) ->
  {ok, Tokens, _} = mongoql_agg_lexer:string(Query),
  mongoql_agg_parser:parse(Tokens);
agg(_Query) ->
  {error, unknow_input}.

agg(Query, Args) -> agg(sf:format(Query, Args)).

%%====================================================================
%% Internal functions
%%====================================================================
