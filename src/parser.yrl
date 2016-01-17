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
  comp_op integer float order_ascending order_descending field string equal_op.

Nonterminals
  filter order grammar expr_list.

Rootsymbol grammar.


grammar -> expr_list : '$1'.

expr_list -> filter : ['$1'].
expr_list -> order : ['$1'].
expr_list -> order expr_list : ['$1' | '$2'].
expr_list -> filter expr_list : ['$1' | '$2'].

filter -> field equal_op field : {unwrap('$1'), {comp_op_conv(unwrap('$2')), unwrap('$3')}}.
filter -> field comp_op field : {unwrap('$1'), {comp_op_conv(unwrap('$2')), unwrap('$3')}}.
filter -> field equal_op integer : {unwrap('$1'), {comp_op_conv(unwrap('$2')), unwrap('$3')}}.
filter -> field comp_op integer : {unwrap('$1'), {comp_op_conv(unwrap('$2')), unwrap('$3')}}.
filter -> field equal_op float : {unwrap('$1'), {comp_op_conv(unwrap('$2')), unwrap('$3')}}.
filter -> field comp_op float : {unwrap('$1'), {comp_op_conv(unwrap('$2')), unwrap('$3')}}.
filter -> field equal_op string : {unwrap('$1'), {comp_op_conv(unwrap('$2')), unwrap('$3')}}.

order -> order_ascending : {unwrap('$1'), 1}.
order -> order_descending : {unwrap('$1'), -1}.


Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

comp_op_conv(<<"<">>) -> <<"$lt">>;
comp_op_conv(<<"<:">>) -> <<"$lte">>;
comp_op_conv(<<":">>) -> <<"$eq">>;
comp_op_conv(<<">:">>) -> <<"$ge">>;
comp_op_conv(<<">">>) -> <<"$gt">>;
comp_op_conv(<<"!:">>) -> <<"$ne">>.
