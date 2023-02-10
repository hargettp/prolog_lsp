:- begin_tests(language_client).

:- use_module(language_client).
:- use_module(language_server).

setup(stdio).

setup(stdio, Connection) :-
  stdio_language_connector(Connector),
  jsonrpc_connect(Connector, Connection).

teardown(stdio).

teardown(Type, Connection) :-
  jsonrpc_disconnect(Connection),
  teardown(Type).

test(initialize,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_R).

test(echo,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_R),
  notify_method(Connection, initialized,[]),
  call_method(Connection,echo,[],[]).


test(methods,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_R),
  notify_method(Connection, initialized,[]),
  call_method(Connection,methods,[],Actual),
  Expected = [
    "initialize",
    "initialized",
    "echo",
    "crash",
    "methods",
    "textDocument/didOpen",
    "textDocument/didChange",
    "textDocument/didClose",
    "shutdown",
    "exit",
    "workspace/symbol"
    ],
  Actual = Expected.

:- end_tests(language_client).
