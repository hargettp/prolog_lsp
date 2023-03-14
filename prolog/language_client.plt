:- begin_tests(language_client).

:- use_module(library(log4p)).
:- use_module(language_client).
:- use_module(language_server).

setup(stdio).

setup(tcp) :-
  catch(
    start_jsonrpc_server(prolog_language_server,3403),
    Exception,
    error("Could not start test server: %t",[Exception])
    ),
  sleep(0.1).

setup(stdio, Connection) :-
  stdio_language_connector(Connector),
  jsonrpc_connect(Connector, Connection).

setup(tcp, Connection) :-
  setup(tcp),
  sleep(0.1),
  jsonrpc_connect(tcp('127.0.0.1':3403),Connection).

teardown(stdio).

teardown(tcp) :-
  stop_jsonrpc_server(prolog_language_server,3403),
  sleep(0.1).

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
  call_method(Connection,initialize, _{},_R),
  notify_method(Connection, initialized,_{}).

% Utilitye

test(echo,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize, _{},_R),
  notify_method(Connection, initialized,_{}),
  Msg = _{msg: "hello!"},
  call_method(Connection,echo,Msg, Msg).


test(methods,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize, _{},_R),
  notify_method(Connection, initialized,_{}),
  call_method(Connection,methods,_{},Actual),
  Expected = [
    "initialize",
    "initialized",
    "echo",
    "crash",
    "methods",
    "$/setTrace",
    "textDocument/didOpen",
    "textDocument/didChange",
    "textDocument/didClose",
    "textDocument/documentSymbol",
    "textDocument/hover",
    "textDocument/references",
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
  call_method(Connection,initialize, _{},_),
  notify_method(Connection, initialized,_{}),
  call_method(Connection,shutdown,_{},_),
  % should get an error
  expect_error(
    call_method(Connection,echo,_{},_{}), 
    jsonrpc_error(_{code: -32600, message: "Invalid state: required initialized, actual shutting_down"})).

test(exit, [
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize, _{},_),
  notify_method(Connection, initialized,_{}),
  call_method(Connection,shutdown,_{},R),
  info('shutdown %w',[R]),
  notify_method(Connection,exit,_{}).

:- end_tests(language_client).
