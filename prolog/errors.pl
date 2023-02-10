:- module(lsp_errors,[
  invalid_state/2
  ]).

:- use_module(library(log4p)).
:- use_module(jsonrpc/server).
:- use_module(jsonrpc/protocol).
:- use_module(jsonrpc/errors).

:- dynamic invalid_state/2.
:- server_error(prolog_language_server, invalid_state(_, _), invalid_state).

invalid_state(_Server, invalid_state(Required, Actual), Error) :-
  swritef(Msg, "Invalid state: required %w, actual %w", [Required, Actual]),
  Error = _{code: -32600, message: Msg }.
