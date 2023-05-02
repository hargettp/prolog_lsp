:- module(jsonrpc_client, [
    jsonrpc_connect/2,
    jsonrpc_disconnect/1,
    with_connection/3,

    call_method/3,
    call_method/4,
    notify_method/3,

    expect_error/2
  ]).

:- use_module(library(uuid)).

:- use_module(library(log4p)).

:- use_module('./protocol').
:- use_module('./connectors').

jsonrpc_connect(ServerInfo,Connection) :-
  connect_to_server(ServerInfo, Connection).

jsonrpc_disconnect(Connection) :-
  close_connection(Connection).

with_connection(ServerInfo,Connection,Goal) :-
  setup_call_cleanup(
    jsonrpc_connect(ServerInfo,Connection),
    Goal,
    jsonrpc_disconnect(Connection)
    ).

call_method(Connection, Method, Params, Result) :-
  connection_stream_pair(Connection,StreamPair),
  stream_pair(StreamPair,In,Out),
  uuid(Id),
  Request = _{jsonrpc: "2.0", id : Id, method: Method, params: Params},
  write_message(Out, Request),
  debug('Sent request: %w',[Request]),
  flush_output(Out),
  read_message(In, Response),
  debug('Received response: %w', [Response]),
  ( _ = Response.get(result) ->
    Result = Response.result ;
    throw(jsonrpc_error(Response.error)) ),
  !.

% Call method but with no params
call_method(Connection, Method, Result) :-
  connection_stream_pair(Connection,StreamPair),
  stream_pair(StreamPair,In,Out),
  uuid(Id),
  Request = _{jsonrpc: "2.0", id : Id, method: Method},
  write_message(Out, Request),
  debug('Sent request: %w',[Request]),
  flush_output(Out),
  read_message(In, Response),
  debug('Received response: %w', [Response]),
  ( _ = Response.get(result) ->
    Result = Response.result ;
    throw(jsonrpc_error(Response.error)) ),
  !.

notify_method(Connection, Method, Params) :-
  connection_stream_pair(Connection,StreamPair),
  stream_pair(StreamPair,_In,Out),
  Request = _{jsonrpc: "2.0", method: Method, params: Params},
  write_message(Out, Request),
  !.

% 
% Helpers, usually for testing
% 

expect_error(Goal, ExpectedError) :-
  catch(
    (Goal, fail),
    ActualError,
    true
    ),
  ( ExpectedError = ActualError
    -> true
    ; ( 
      warn("Expected error %q, ecnountered %q",[ExpectedError, ActualError]), 
      fail
      )
    ).