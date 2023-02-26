:- begin_tests(language_client).

:- use_module(library(log4p)).
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

% -------------------------------------
% Tests

%  Initialization
test(initialize,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_R),
  notify_method(Connection, initialized,[]).

% Utilitye

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

% Shutdown
test(shutdown, [
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_),
  notify_method(Connection, initialized,[]),
  call_method(Connection,shutdown,[],_),
  % should get an error
  expect_error(
    call_method(Connection,echo,[],[]), 
    jsonrpc_error(_{code: -32600, message: "Invalid state: required initialized, actual shutting_down"})).

test(exit, [
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_),
  notify_method(Connection, initialized,[]),
  call_method(Connection,shutdown,[],_),
  call_method(Connection,exit,[],_).
  % notify_method(Connection, exit,[]).

:- end_tests(language_client).
