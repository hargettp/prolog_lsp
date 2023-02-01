:- module(jsonrpc_connectors, [
  start_connector/1,
  stop_connector/1
  ]
).

:- dynamic start_connector/1.
:- dynamic stop_connector/1.