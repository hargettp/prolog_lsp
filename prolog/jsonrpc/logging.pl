:- module(jsonrpc_logging, [
  set_jsonrpc_log_directory/1,
  get_jsonrpc_log_directory/1,

  enable_jsonrpc_logging/1,
  enable_jsonrpc_logging/0,

  disable_jsonrpc_logging/0,
  clear_jsonrpc_logs/0,

  enable_jsonrpc_tracing/0,
  disable_jsonrpc_tracing/0
  ]).

:- use_module(library(filesex)).
:- use_module(library(log4p)).
:- use_module(library(http/json)).

:- dynamic jsonrpc_log_directory/1.
:- dynamic jsonrpc_log_enabled/0.
:- dynamic jsonrpc_trace_enabled/0.

set_jsonrpc_log_directory(LogDirectory) :-
  retractall(jsonrpc_log_directory(_)),
  assertz(jsonrpc_log_directory(LogDirectory)),
  debug("JSON-RPC log directory set to %w",[LogDirectory]).

get_jsonrpc_log_directory(LogDirectory) :-
  jsonrpc_log_directory(LogDirectory).

enable_jsonrpc_logging(LogDirectory) :-
  set_jsonrpc_log_directory(LogDirectory),
  enable_jsonrpc_logging.

enable_jsonrpc_logging :-
  jsonrpc_log_enabled, !.

enable_jsonrpc_logging :-
  assertz(jsonrpc_log_enabled),
  debug("JSON_RPC logging enabled").

disable_jsonrpc_logging :-
  retractall(jsonrpc_log_enabled),
  debug("JSON_RPC logging disabled").

clear_jsonrpc_logs :-
  get_jsonrpc_log_directory(LogDirectory),
  delete_directory_contents(LogDirectory).

enable_jsonrpc_tracing :-
  assertz(jsonrpc_trace_enabled),
  debug("JSON_RPC tracing enabled").

disable_jsonrpc_tracing :-
  retractall(jsonrpc_trace_enabled),
  debug("JSON_RPC tracing disabled").

% -- hooks ---

jsonrpc_protocol:on_message_read(Message) :-
  log_message(Message, request).

jsonrpc_protocol:on_message_write(Message) :-
  log_message(Message, response).

jsonrpc_protocol:on_message_read(Message) :-
  trace_message(Message, request).

jsonrpc_protocol:on_message_write(Message) :-
  trace_message(Message, response).

% --- helpers ---

log_message(Message, Kind) :-
  logging_enabled,
  build_filename(Kind, FileName),
  get_jsonrpc_log_directory(LogDirectory),
  (exists_directory(LogDirectory) 
    -> true
    ; make_directory(LogDirectory) 
    ),
  absolute_file_name(FileName, Absolute, [relative_to(LogDirectory)]),
  atom_json_dict(Content, Message, [as(string)]),
  setup_and_call_cleanup(
    open(Absolute, write, Out, [create([read,write])]),
    write(Out, Content),
    close(Out)
    ).

build_filename(Kind, FileName) :-
  get_time(Timestamp),
  format_time(string(TSString), '%Y%m%d_%H%M%S',Timestamp),
  swritef(FileName,"%w.%w.log.json",[TSString, Kind]).

logging_enabled :-
  jsonrpc_log_enabled,
  jsonrpc_log_directory(LogDirectory),
  ground(LogDirectory).

trace_message(Message, request) :-
  tracing_enabled,
  atom_json_dict(Content, Message, [as(string)]),
  debug('received %w', [Content]).

trace_message(Message, response) :-
  tracing_enabled,
  atom_json_dict(Content, Message, [as(string)]),
  debug('sent %w', [Content]).

tracing_enabled :-
  jsonrpc_trace_enabled.
