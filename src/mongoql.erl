-module('mongoql').

%% API exports
-export([parse/1]).

-export_type([query/0]).

%%%_ * Types -----------------------------------------------------------

-type query() :: list() | binary().

%%====================================================================
%% API functions
%%====================================================================

% init() ->
%   % compile lexer
%   leex:file(lexer),
%   c(lexer),
%   % compile parser
%   yecc:file(parser),
%   c(parser).

-spec parse(query()) -> {ok, list()} | {error, unknow_input}.
parse(Query) when is_binary(Query) ->
  parse(binary_to_list(Query));
parse(Query) when is_list(Query) ->
  {ok, Tokens, _} = lexer:string(Query),
  parser:parse(Tokens);
parse(_Query) ->
  {error, unknow_input}.

%%====================================================================
%% Internal functions
%%====================================================================
