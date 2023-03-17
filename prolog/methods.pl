:- module(lsp_methods,[

  ]).

:- use_module(library(log4p)).
:- use_module(jsonrpc/server).
:- use_module(capabilities).
:- use_module(errors).
:- use_module(symbols).
:- use_module(pls_index).

% Initialization
:- server_method(prolog_language_server, initialize, pls_initialize).
:- server_method(prolog_language_server, initialized, pls_initialized).

% Utility
:- server_method(prolog_language_server, echo, pls_echo).
:- server_method(prolog_language_server, crash, pls_crash).
:- server_method(prolog_language_server, methods, pls_methods).
:- server_method(prolog_language_server, '$/setTrace', pls_trace).

% Text Document Synchronization
:- server_method(prolog_language_server, 'textDocument/didOpen', pls_text_document_did_open).
:- server_method(prolog_language_server, 'textDocument/didChange', pls_text_document_did_change).
:- server_method(prolog_language_server, 'textDocument/didClose', pls_text_document_did_close).

% Language features
:- server_method(prolog_language_server, 'textDocument/documentSymbol', pls_document_document_symbol).
:- server_method(prolog_language_server, 'textDocument/hover', pls_text_document_hover).
:- server_method(prolog_language_server, 'textDocument/references', pls_text_document_references).
:- server_method(prolog_language_server, 'textDocument/definition', pls_text_document_definition).

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
  TraceLevel = Params.get(trace, "off"),
  set_trace_level(TraceLevel),
  \+ get_server_state(Server, _),
  server_capabilities(Capabilities),
  Result = _{
    capabilities: Capabilities,
    serverInfo: _{name: "Prolog"}
    },
  set_server_state(Server, initializing).

pls_initialized(Server, _Params) :-
  require_server_state(Server, initializing),
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

pls_trace(_Server, Params) :-
  TraveLevel = Params.value,
  set_trace_level(TraveLevel).

% Text Document sync
pls_text_document_did_open(_Server, Params) :-
  Document = Params.textDocument,
  Document = _{
    uri: URI,
    languageId: Language,
    version: Version,
    text: Content
  },
  % Check its a prolog document
  store_document(URI, Language, Version, Content),
  index_text(URI).

pls_text_document_did_change(_Server, Params) :-
  Document = Params.textDocument,
  Changes = Params.contentChanges,
  URI = Document.uri,
  Content = Changes.text,
  get_document_property(URI, language(Language)),
  store_document(URI, Language, Document.version, Content),
  index_text(URI).

pls_text_document_did_close(_Server, Params) :-
  Document = Params.textDocument,
  URI = Document.uri,
  clear_document_content(URI).

% Other text document

pls_document_document_symbol(_Server, Result, Params) :-
  Document = Params.textDocument,
  URI = Document.uri,
  document_symbols(URI, Result).

pls_text_document_hover(_Server, Result, _Param) :-
  Document = Params.textDocument,
  URI = Document.uri,
  Position = Params.position,
  hover_for_position(URI, Position, Hover),
  Result = Hover.

pls_text_document_definition(_Server, Result, Params) :-
  Document = Params.textDocument,
  URI = Document.uri,
  Position = Params.position,
  info("Lookingfrom %w for definition at %q",[URI, Position]),
  definition_for_position(URI, Position, References),
  info("Found from %w definition %q",[URI, References]),
  Result = References.

% Find references for defined predicates
pls_text_document_references(_Server, Result, Params) :-
  Document = Params.textDocument,
  URI = Document.uri,
  Position = Params.position,
  references_for_position(URI, Position, References),
  info("Found in %w references %q",[URI, References]),
  Result = References.

% Shutdown

pls_shutdown(Server, Result, _Params) :-  
  require_server_state(Server, initialized),
  % we don't actually shut anything down right now
  Result = _{},
  set_server_state(Server, shutting_down).

pls_exit(Server, _Params) :-
  require_server_state(Server, shutting_down),
  % we don't actually exit anything down right now
  request_exit_server(Server).

pls_workspace_symbols(_Server, Symbols, Params) :-
  workspace_symbols(Params.query, Symbols).

% --- helpers ---

set_trace_level("off") :-
  log4p:set_global_log_level(info).

set_trace_level("messages") :-
  log4p:set_global_log_level(debug).

set_trace_level("verbose") :-
  log4p:set_global_log_level(trace).