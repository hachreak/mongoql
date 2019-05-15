%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2019 Leonardo Rossi
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
  comp_op integer float timestamp field
  string equal_op in_op square_bracket_open_op square_bracket_close_op
  not_op exists_op group_op round_bracket_open_op round_bracket_close_op
  dollar_op curly_bracket_open_op curly_bracket_close_op.

Nonterminals
  filter grammar filters variable variables compare_op compare
  aggs agg agg_value groups.

Rootsymbol grammar.


grammar -> filters groups: query('$1', '$2').

groups -> group_op aggs : '$2'.

filters -> filter : ['$1'].
filters -> filter filters : ['$1' | '$2'].

filter -> field compare : {unwrap('$1'), '$2'}.
filter -> not_op field compare : uni_op(unwrap('$1'), unwrap('$2'), '$3').

compare -> compare_op variable : comp_op_conv(var, unwrap('$1'), unwrap('$2')).
compare -> compare_op field : comp_op_conv(field, unwrap('$1'), unwrap('$2')).
compare -> in_op square_bracket_open_op variables square_bracket_close_op :
        {in_op_conv(unwrap('$1')), unwrap('$3')}.
compare -> exists_op : {'$exists', true}.

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
agg -> field equal_op curly_bracket_open_op aggs curly_bracket_close_op :
        {unwrap('$1'), maps:from_list('$4')}.

agg_value -> dollar_op field round_bracket_open_op field round_bracket_close_op :
      {dollar(unwrap('$2')), dollar(unwrap('$4'))}.
agg_value -> dollar_op field round_bracket_open_op integer round_bracket_close_op :
      {dollar(unwrap('$2')), to_binary(unwrap('$4'))}.
agg_value -> field : dollar(unwrap('$1')).


Erlang code.

query(Filters, Groups) -> [
  {'$match', {'$and', Filters}},
  {'$group', maps:from_list(Groups)}
].

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

to_binary(Int) when is_integer(Int) ->
  to_binary(integer_to_list(Int));
to_binary(List) when is_list(List) ->
  list_to_binary(List).
