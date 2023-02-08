:- module(errors,[

  ]).

:- use_module(library(log4p)).
:- use_module(jsonrpc/server).
:- use_module(jsonrpc/protocol).

:- server_error(prolog_language_server, invalid_state(_, _), invalid_state).

invalid_state(_Server, invalid_state(Required, Actual), Error) :-
  swritef(string(Msg), "Invalid state: required %w, actual %w", [Required, Actual]),
  Error = _{code: -32600, message: Msg }.
