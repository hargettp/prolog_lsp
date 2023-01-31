% :- begin_tests(jsonrpc_client,[setup(setup),cleanup(teardown)]).

% :- use_module(jsonrpc_protocol).
% :- use_module(jsonrpc_client).
% :- use_module(jsonrpc_server).

% :- use_module(library(log4p)).

% :- server_method(jsonrpc_client_test, echo, jsonrpc_server:echo).
% :- server_method(jsonrpc_client_test, crash, jsonrpc_server:crash).

% setup :-
%   catch(
%     start_jsonrpc_server(jsonrpc_client_test,3401),
%     Exception,
%     error("Could not start test server: %t",[Exception])
%     ),
%   sleep(0.25).

% setup(Connection) :-
%   jsonrpc_connect('127.0.0.1':3401,Connection).

% teardown :-
%   sleep(0.25),
%   stop_jsonrpc_server(jsonrpc_client_test,3401).

% teardown(Connection) :-
%   jsonrpc_disconnect(Connection).

% test(echo,[setup(setup(Connection)),cleanup(teardown(Connection))]) :-
%   call_method(Connection,echo,[],[]).

% test(missing,[setup(setup(Connection)),cleanup(teardown(Connection))]) :-
%   catch(
%     call_method(Connection,missing,[],[]),
%     jsonrpc_error(Error),
%     (Error.code = -32601, Error.data = "missing" , Error.message = "Method not found")
%     ).

% test(crash,[setup(setup(Connection)),cleanup(teardown(Connection))]) :-
%   catch(
%     call_method(Connection,crash,[],[]),
%     jsonrpc_error(Error),
%     (Error.code = -32000, Error.message = "An unknown error occurred")
%     ).

% test(parse_error,[setup(setup(Connection)),cleanup(teardown(Connection))]) :-
%   catch(
%     call_parse_error(Connection),
%     jsonrpc_error(Error),
%     (Error.code = -32700, Error.message = "Parse error")
%     ).

% test(invalid_request,[setup(setup(Connection)),cleanup(teardown(Connection))]) :-
%   catch(
%     call_invalid_request(Connection),
%     jsonrpc_error(Error),
%     (Error.code = -32600, Error.message = "Invalid Request")
%     ).

% call_parse_error(Connection) :-
%   RawMessage = '{ "jsonrpc" : "2.0", "id" : 1, "method", "echo", "params": [] }',
%   call_raw(Connection, RawMessage).

% call_invalid_request(Connection) :-
%   RawMessage = '{ "id" : 1, "method" : "echo", "params": [] }',
%   call_raw(Connection, RawMessage).

% call_raw(Connection, RawMessage) :-
%   connection(_,StreamPair) = Connection,
%   stream_pair(StreamPair, In,Out),
%   atom_length(RawMessage,ContentLength),
%   format(Out,"Content-Length: ~d\r\n\r\n",[ContentLength]),
%   write(Out,RawMessage),
%   write(Out,'\r\n'),
%   flush_output(Out),
%   read_header(In, Size),
%   read_message(In, Size, Response),
%   ( _ = Response.get(result) ->
%     _Result = Response.result ;
%     throw(jsonrpc_error(Response.error)) ),
%   !.

% :- end_tests(jsonrpc_client).
