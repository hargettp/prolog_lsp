:- module(jsonrpc_connectors, [
  connect_to_server/2,
  close_connection/1,
  serve_messages/1
  ]
).

% 
% Connectors
% 
:- reexport(connectors/stdio).
:- reexport(connectors/tcp).

% 
% Client methods
% 

% connect_to_server(Server, Connection)
:- multifile connect_to_server/2.

% close_connection(Connnection)
:- multifile close_connection/1.

% 
% Server methods
% 

% serve_messages(Server)
:- multifile serve_messages/1.

