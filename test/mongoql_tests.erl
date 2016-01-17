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
%%% @doc MongoQL API - Tests
%%% @end

-module(mongoql_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

mongoql_test_() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun (SetupData) ->
        [
         query(SetupData)
        ]
    end
  }.

start() ->
  ok.

stop(_SetupData) ->
  ok.

query(_) ->
  fun() ->
      Query = "house.temperature>23 house.city:\"Milano\" house.pression<:1015 +house.name",
      {ok, Result} = mongoql:parse(Query),
      Expect = {'$query',{'$and',[{<<"house.temperature">>,
                                   {<<"$gt">>,23}},
                                  {<<"house.city">>,{<<"$eq">>,<<"Milano">>}},
                                  {<<"house.pression">>,{<<"$lte">>,1015}}]},
                '$orderby',
                [{<<"house.name">>,1}]},
      ?assertEqual(Result, Expect)
  end.
