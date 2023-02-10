:- module(lsp_methods,[

  ]).

:- use_module(library(log4p)).
:- use_module(jsonrpc/server).
:- use_module(code).
:- use_module(workspace).

% Initialization
:- server_method(prolog_language_server, initialize, pls_initialize).
:- server_method(prolog_language_server, initialized, pls_initialized).

% Utility
:- server_method(prolog_language_server, echo, pls_echo).
:- server_method(prolog_language_server, crash, pls_crash).
:- server_method(prolog_language_server, methods, pls_methods).


% Text Document Synchronization
:- server_method(prolog_language_server, 'textDocument/didOpen', pls_text_document_did_open).
:- server_method(prolog_language_server, 'textDocument/didChange', pls_text_document_did_change).
:- server_method(prolog_language_server, 'textDocument/didClose', pls_text_document_did_close).

% Shutdown
:- server_method(prolog_language_server, shutdown, pls_shutdown).
:- server_method(prolog_language_server, exit, pls_exit).

:- server_method(prolog_language_server, 'workspace/symbol', pls_workspace_symbols).

% 
% State
% 

% language_server_state(Server, State)
:- dynamic language_server_state/2.

get_server_state(Server, State) :-
  language_server_state(Server, State).

set_server_state(Server, State) :-
  retractall(language_server_state(Server, _)),
  debug("Setting server %w state to %w", [Server, State]),
  assertz(language_server_state(Server, State)),
  debug("Server %w state set to %w", [Server, State]).

require_server_state(Server, Required) :-
  get_server_state(Server, Required), 
  debug('for %w required state of %w is met', [Server, Required]),
  !.

require_server_state(Server, Required) :-
  get_server_state(Server, State),
  debug('for %w required state of %w but is %w', [Server, Required, State]),
  throw(invalid_state(Required, State)).

% -------------------
% 
% Methods
% 
% - - - - - - - - - -

%
% Initialization
% 

pls_initialize(Server, Result,Params) :-
  \+ get_server_state(Server, _),
  info("Method initialize called"),
  info("Client capabilities: %w",[Params]),
  server_capabilities(Capabilities),
  Result = _{
    capabilities: Capabilities
    },
  set_server_state(Server, initializing).

pls_initialized(Server, _Result,_Params) :-
  require_server_state(Server, initializing),
  info("Notification initialized called"),
  set_server_state(Server, initialized).

% 
% Utility
% 

pls_echo(Server, Result, Params) :-
  require_server_state(Server, initialized),
  jsonrpc_server:echo(Server, Result, Params).

pls_crash(Server, Result, Params) :-
  require_server_state(Server, initialized),
  jsonrpc_server:crash(Server, Result, Params).

pls_methods(Server, Result, _Params) :-
  % require_server_state(Server, initialized),
  findall(
    Method, 
    clause(jsonrpc_methods:declared_server_method(_:Server, _:Method, _:_),_),
    Methods
    ),
  Result = Methods.

% Text Document sync
pls_text_document_did_open(_Server, _Result,_Params).
pls_text_document_did_change(_Server, _Result,_Params).
pls_text_document_did_close(_Server, _Result,_Params).

% Shutdown

pls_shutdown(_Server, Result) :-
  require_server_state(Server, initialized),
  info("Method shutdown called"),
  % we don't actually shut anything down right now
  Result = null,
  set_server_state(Server, shutting_down).

pls_exit(Server, Result) :-
  require_server_state(Server, shutting_down),
  info("Method exit called"),
  % we don't actually exit anything down right now
  Result = null,
  halt.

pls_workspace_symbols(_Server, Symbols, Query) :-
  workspace_symbols(Query, Symbols).

% common data structures
server_capabilities(Capabilities) :-
  Capabilities = _{
    textDocumentSync: _{
      openClose: true,
      % This means the client sends the full content on each change
      change: 1
    }
  }.
