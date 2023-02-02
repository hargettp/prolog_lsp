:- module(jsonrpc_client, [
    jsonrpc_connect/2,
    jsonrpc_disconnect/1,
    with_connection/3,

    call_method/4,
    notify_method/3
  ]).

:- use_module(library(uuid)).

:- use_module(library(log4p)).

:- use_module('./jsonrpc_protocol').
:- use_module('./connectors').

jsonrpc_connect(Connector,Connection) :-
  connect_to_server(Connector, Connection).

jsonrpc_disconnect(Connection) :-
  close_connection(Connection).

with_connection(ServerAddress,Connection,Goal) :-
  setup_call_cleanup(
    jsonrpc_connect(ServerAddress,Connection),
    Goal,
    jsonrpc_disconnect(Connection)
    ).

call_method(Connection, Method, Params, Result) :-
  connection(_,StreamPair) = Connection,
  stream_pair(StreamPair,In,Out),
  uuid(Id),
  Request = _{jsonrpc: "2.0", id : Id, method: Method, params: Params},
  write_message(Out, Request),
  read_message(In, Response),
  ( _ = Response.get(result) ->
    Result = Response.result ;
    throw(jsonrpc_error(Response.error)) ),
  !.

notify_method(Connection, Method, Params) :-
  connection(_,StreamPair) = Connection,
  stream_pair(StreamPair,_In,Out),
  Request = _{jsonrpc: "2.0", method: Method, params: Params},
  write_message(Out, Request),
  !.
