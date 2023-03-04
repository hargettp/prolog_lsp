:- module(language_client, [
  stdio_language_connector/1,
  with_stdio_language/2,
  with_tcp_language/3,

  initialize/2,
  initialized/1,
  shutdown/2,
  exit/1
  ]).

:- reexport('./jsonrpc/client').

stdio_language_connector(stdio(Program, Args)) :-
  Program = path(swipl),
  Args = [
    '-s',
    'run.pl',
    '-g',
    'run_stdio_language_server'
  ].

with_stdio_language(Connection, Goal) :-
  stdio_language_connector(Connector),
  with_connection(Connector, Connection, Goal).

tcp_language_connector(Port, tcp('127.0.0.1':Port)).

with_tcp_language(Port, Connection, Goal) :-
  tcp_language_connector(Port, Connector),
  with_connection(Connector, Connection, Goal).

initialize(Connection,Result) :-
  call_method(Connection,initialize,[],Result).

initialized(Connection) :-
  notify_method(Connection,initialized,[]).

shutdown(Connection,Result) :-
  call_method(Connection,shutdown,[],Result).

exit(Connection) :-
  notify_method(Connection,shutdown,[]).
