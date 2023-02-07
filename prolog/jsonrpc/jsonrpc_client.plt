:- begin_tests(jsonrpc_client).

:- use_module(jsonrpc_protocol).
:- use_module(jsonrpc_client).
:- use_module(jsonrpc_server).
:- use_module(connectors).

:- use_module(library(log4p)).

:- server_method(jsonrpc_client_test, echo, jsonrpc_server:echo).
:- server_method(jsonrpc_client_test, crash, jsonrpc_server:crash).

setup(tcp) :-
  catch(
    start_jsonrpc_server(jsonrpc_client_test,3401),
    Exception,
    error("Could not start test server: %t",[Exception])
    ),
  sleep(0.25).

setup(stdio).

setup(tcp, Connection) :-
  setup(tcp),
  sleep(0.25),
  jsonrpc_connect(tcp('127.0.0.1':3401),Connection).

setup(stdio, Connection) :-
  Program = path(swipl),
  Args = [
    '-s',
    'run.pl',
    '-g',
    'run_stdio_language_server'
  ],
  jsonrpc_connect(stdio(Program, Args), Connection).

teardown(tcp) :-
  stop_jsonrpc_server(jsonrpc_client_test,3401),
  sleep(0.25).

teardown(stdio).

teardown(Type, Connection) :-
  jsonrpc_disconnect(Connection),
  teardown(Type).

test(echo,[forall(member(Type, [stdio, tcp])), setup(setup(Type, Connection)),cleanup(teardown(Type, Connection))]) :-
  call_method(Connection,echo,[],[]).

test(missing,[forall(member(Type, [stdio, tcp])), setup(setup(Type, Connection)),cleanup(teardown(Type, Connection))]) :-
  catch(
    call_method(Connection,missing,[],[]),
    jsonrpc_error(Error),
    (Error.code = -32601, Error.data = "missing" , Error.message = "Method not found")
    ).

test(crash,[forall(member(Type, [stdio, tcp])), setup(setup(Type, Connection)),cleanup(teardown(Type, Connection))]) :-
  catch(
    call_method(Connection,crash,[],[]),
    jsonrpc_error(Error),
    (Error.code = -32000, Error.message = "An unknown error occurred")
    ).

test(parse_error,[forall(member(Type, [stdio, tcp])), setup(setup(Type, Connection)),cleanup(teardown(Type, Connection))]) :-
  catch(
    call_parse_error(Connection),
    jsonrpc_error(Error),
    (Error.code = -32700, Error.message = "Parse error")
    ).

test(invalid_request,[forall(member(Type, [stdio, tcp])), setup(setup(Type, Connection)),cleanup(teardown(Type, Connection))]) :-
  catch(
    call_invalid_request(Connection),
    jsonrpc_error(Error),
    (Error.code = -32600, Error.message = "Invalid Request")
    ).

% 
% Helpers
% 

call_parse_error(Connection) :-
  RawMessage = '{ "jsonrpc" : "2.0", "id" : 1, "method", "echo", "params": [] }',
  call_raw(Connection, RawMessage).

call_invalid_request(Connection) :-
  RawMessage = '{ "id" : 1, "method" : "echo", "params": [] }',
  call_raw(Connection, RawMessage).

call_raw(Connection, RawMessage) :-
  connection_stream_pair(Connection, StreamPair),
  stream_pair(StreamPair, In,Out),
  string_length(RawMessage,ContentLength),
  format(Out,"Content-Length: ~d\r\n\r\n",[ContentLength]),
  write(Out,RawMessage),
  write(Out,'\r\n'),
  flush_output(Out),
  read_message(In, Response),
  ( _ = Response.get(result) ->
    _Result = Response.result ;
    throw(jsonrpc_error(Response.error)) ),
  !.

:- end_tests(jsonrpc_client).
