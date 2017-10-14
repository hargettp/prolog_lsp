:- module(language_client, [
  with_language/3,
  with_local_language/3,

  initialize/2,
  initialized/1,
  shutdown/2,
  exit/1
  ]).

:- use_module(library(jsonrpc/jsonrpc_client)).

with_language(ServerAddress, Connection, Goal) :-
  with_connection(ServerAddress, Connection, Goal).

with_local_language(Port, Connection, Goal) :-
  with_local_connection(Port, Connection, Goal).

initialize(Connection,Result) :-
  call_method(Connection,initialize,[],Result).

initialized(Connection) :-
  notify_method(Connection,initialized,[]).

shutdown(Connection,Result) :-
  call_method(Connection,shutdown,[],Result).

exit(Connection) :-
  notify_method(Connection,shutdown,[]).
