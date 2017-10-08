:- module(jsonrpc_client, [
    jsonrpc_connect/2,
    jsonrpc_disconnect/1,

    call_method/4
  ]).

:- use_module(library(uuid)).

:- use_module(library(log4p)).

:- use_module(library(jsonrpc/jsonrpc_protocol)).

jsonrpc_connect(ServerAddress,Connection) :-
  tcp_connect(ServerAddress,StreamPair,[]),
  Connection = connection(ServerAddress,StreamPair).

jsonrpc_disconnect(connection(_,StreamPair)) :-
  ignore(close(StreamPair)).

call_method(Connection, Method, Params, Result) :-
  connection(_,StreamPair) = Connection,
  stream_pair(StreamPair,In,Out),
  uuid(Id),
  Request = _{jsonrpc: "2.0", id : Id, method: Method, params: Params},
  write_message(Out, Request),
  read_header(In, Size),
  read_message(In, Size, Response),
  ( _ = Response.get(result) ->
    Result = Response.result ;
    throw(jsonrpc_error(Response.error)) ),
  !.
