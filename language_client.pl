:- module(language_client, [
  with_language/3,

  initialize/2,
  initialized/1,
  shutdown/2,
  exit/1
  ]).

:- use_module(lib(jsonrpc/jsonrpc_client)).

with_language(ServerInfo, Connection, Goal) :-
  with_connection(ServerInfo, Connection, Goal).

initialize(Connection,Result) :-
  call_method(Connection,initialize,[],Result).

initialized(Connection) :-
  notify_method(Connection,initialized,[]).

shutdown(Connection,Result) :-
  call_method(Connection,shutdown,[],Result).

exit(Connection) :-
  notify_method(Connection,shutdown,[]).
