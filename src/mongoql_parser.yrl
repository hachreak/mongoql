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
%%% @doc Parser
%%% @end

Terminals
  comp_op integer float timestamp order_ascending order_descending field
  string equal_op in_op square_bracket_open_op square_bracket_close_op
  not_op exists_op agg_op round_bracket_open_op round_bracket_close_op
  match_op.

Nonterminals
  filter order grammar filters orders variable variables compare_op compare
  aggs agg agg_value.

Rootsymbol grammar.


grammar -> filters orders : query('$1', '$2').
grammar -> filters : filters('$1').
grammar -> orders : orders('$1').
grammar -> agg_op aggs match_op filters : [{'$group', '$2'}, {'$match', '$4'}].
grammar -> agg_op aggs : [{'$group', '$2'}].

filters -> filter : ['$1'].
filters -> filter filters : ['$1' | '$2'].

orders -> order : ['$1'].
orders -> order orders : ['$1' | '$2'].

filter -> field compare : {unwrap('$1'), '$2'}.
filter -> not_op field compare : uni_op(unwrap('$1'), unwrap('$2'), '$3').

compare -> compare_op variable : comp_op_conv(var, unwrap('$1'), unwrap('$2')).
compare -> compare_op field : comp_op_conv(field, unwrap('$1'), unwrap('$2')).
compare -> in_op square_bracket_open_op variables square_bracket_close_op : {in_op_conv(unwrap('$1')), unwrap('$3')}.
compare -> exists_op : {'$exists', true}.

order -> order_ascending : {unwrap('$1'), 1}.
order -> order_descending : {unwrap('$1'), -1}.

variables -> variable : [unwrap('$1')].
variables -> variable variables : [unwrap('$1') | '$2'].

variable -> timestamp : '$1'.
variable -> string : '$1'.
variable -> integer : unwrap('$1').
variable -> float : unwrap('$1').

compare_op -> equal_op : '$1'.
compare_op -> comp_op : '$1'.

% Aggregation queries

aggs -> agg: ['$1'].
aggs -> agg aggs: ['$1' | '$2'].

agg -> field equal_op agg_value : {unwrap('$1'), '$3'}.

agg_value -> field round_bracket_open_op field round_bracket_close_op : {dollar(unwrap('$1')), dollar(unwrap('$3'))}.
agg_value -> field : {dollar(unwrap('$1'))}.


Erlang code.

query(Filters, Orders) -> {'$query', {'$and', Filters}, '$orderby', Orders}.
filters(Filters) -> {'$and', Filters}.
orders(Orders) -> {'$orderby', Orders}.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V;
unwrap(V) -> V.

uni_op(<<"not">>, Name, Rest) ->
  case Rest of
    {'$exists', true} -> {Name, {'$exists', false}};
    _ -> {Name, {'$not', Rest}}
  end.

comp_op_conv(_, <<"<">>, Var) -> {'$lt', Var};
comp_op_conv(_, <<"<:">>, Var) -> {'$lte', Var};
comp_op_conv(_, <<":">>, Var) -> {'$eq', Var};
comp_op_conv(_, <<">:">>, Var) -> {'$gte', Var};
comp_op_conv(_, <<">">>, Var) -> {'$gt', Var};
comp_op_conv(_, <<"!:">>, Var) -> {'$ne', Var};
comp_op_conv(var, <<"~">>, Var) ->
  case re:run(Var, "^[\-0-9A-Za-z^$\.]+$") of
    nomatch -> throw(invalid_regex);
    _ -> {'$regex', Var}
  end;
comp_op_conv(_, _, _) -> throw(invalid_regex).

in_op_conv(<<"in">>) -> '$in'.

dollar(Value) -> << <<"$">>/binary, Value/binary >>.
