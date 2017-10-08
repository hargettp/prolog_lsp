:- begin_tests(jsonrpc_client,[setup(setup),cleanup(teardown)]).

:- use_module(jsonrpc_client).

:- use_module(jsonrpc_server).

:- use_module(library(log4p)).

:- method(jsonrpc_client_test, echo, jsonrpc_server:echo).
:- method(jsonrpc_client_test, crash, jsonrpc_server:crash).

setup :-
  start_jsonrpc_server(jsonrpc_client_test,3401),
  sleep(0.25).

teardown :-
  sleep(0.25),
  stop_jsonrpc_server(jsonrpc_client_test,3401).

setup(Connection) :-
  jsonrpc_connect('127.0.0.1':3401,Connection).

teardown(Connection) :-
  jsonrpc_disconnect(Connection).

test(echo,[setup(setup(Connection)),cleanup(teardown(Connection))]) :-
  call_method(Connection,echo,[],[]).

test(missing,[setup(setup(Connection)),cleanup(teardown(Connection))]) :-
  catch(
    call_method(Connection,missing,[],[]),
    jsonrpc_error(Error),
    (Error.code = -32601, Error.data = "missing" , Error.message = "Method not found")
    ).

test(crash,[setup(setup(Connection)),cleanup(teardown(Connection))]) :-
  catch(
    call_method(Connection,crash,[],[]),
    jsonrpc_error(Error),
    (Error.code = -32000, Error.message = "An unknown error occurred")
    ).

:- end_tests(jsonrpc_client).
