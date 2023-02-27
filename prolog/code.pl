:- module(code,[
  index_workspace/1
  ]).

:- use_module(library(prolog_xref)).
:- use_module(library(uri)).

:- use_module(library(log4p)).

:- use_module(symbols).

index_workspace(WorkspaceUri) :-
  findall(
    SourceUri,
    (workspace_source_file(WorkspaceUri, SourceUri),index_source(SourceUri)),
    _Sources).

index_source(SourceUri) :-
  uri_file_name(SourceUri,Source),
  info("Indexing source %t",[Source]),
  xref_source(Source).
