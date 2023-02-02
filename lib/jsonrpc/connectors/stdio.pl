:- module(jsonrpc_connectors_stdio,[
  create_stdio_server/2
  ]).

:- use_module('../jsonrpc_server').

% 
% Client methods
% 

connect_to_server(stdio(Program, Args), stdio_connection(ChildPID, StreamPair)) :-
  process_create(
    Program, 
    Args, 
    [
      process(ChildPID),
      stdin(ConnectionIn),
      stdout(ConnectionOut),
      stderr(pipe(std))
      ]
    ),
  stream_pair(StreamPair, ConnectionIn, ConnectionOut).

close_connection(stdio_connection(ChildPID, _StreamPair)) :-
  process_kill(ChildPID, quit),
  process_wait(ChildPID, Status, [timeout(0)]),
  Status = timeout 
    -> true
    ; (
      sleep(5),
      process_wait(ChildPID, Status, [timeout(0)]),
      ( Status = timeout -> process_kill(ChildPID, term) ; true )
      ).

% 
% Server methods
% 

create_stdio_server(ServerName, stdio_server(ServerName, StreamPair)) :-
  current_input(In),
  current_output(Out),
  stream_pair(StreamPair, In, Out).

serve_messages(stdio_server(ServerName, StreamPair)) :-
  handle_connection(ServerName, stdio, StreamPair).

  

