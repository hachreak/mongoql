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

test_query(Query, Expect) ->
  {ok, Result} = mongoql:parse(Query),
  ?assertEqual(Result, Expect).

query(_) ->
  fun() ->
      test_query(
        "house.temperature>23 house.city:\"Milano\" house.pression<:1015 house.name-asc",
        {'$query',{'$and',[{<<"house.temperature">>, {'$gt',23}},
                           {<<"house.city">>,{'$eq',<<"Milano">>}},
                           {<<"house.pression">>,{'$lte',1015}}]},
         '$orderby',
         [{<<"house.name">>,1}]}),

      test_query("a > 2016-01-15T18:19:28Z",
                 {'$query',{'$and',[{<<"a">>,{'$gt',{1452,881968,0}}}]}}),

      test_query("a > 2", {'$query',{'$and',[{<<"a">>,{'$gt',2}}]}}),

      test_query("a <: -2", {'$query',{'$and',[{<<"a">>,{'$lte',-2}}]}}),

      test_query("a >: -2", {'$query',{'$and',[{<<"a">>,{'$gte',-2}}]}}),

      test_query("a < -23.52", {'$query',{'$and',[{<<"a">>,{'$lt',-23.52}}]}}),

      test_query("a : \"Hello World!\"",
                 {'$query',{'$and',[{<<"a">>,{'$eq', <<"Hello World!">>}}]}}),

      test_query("a-asc", {'$orderby', [{<<"a">>, 1}]}),

      test_query("a-desc", {'$orderby', [{<<"a">>, -1}]}),

      test_query("a-desc b-asc", {'$orderby', [{<<"a">>, -1}, {<<"b">>, 1}]}),

      test_query(<<"a!:2 b-asc">>,
                 {'$query', {'$and', [{<<"a">>, {'$ne', 2}}]},
                 '$orderby', [{<<"b">>, 1}]})
  end.
