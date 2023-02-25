:- module(pls_index_indexing, [
  index_text/1,

  start_index_roots/1,

  index_roots/1,
  index_root/1,

  index_defined/3
  ]).

:- use_module(library(log4p)).
:- use_module(library(option)).
:- use_module(library(prolog_source)).
:- use_module(library(uri)).

:- use_module(documents).

with_input_from(String, Goal) :-
  setup_and_call_cleanup(
    open_string(String, In),
    setup_and_call_cleanup(
      current_input(SaveIn),
      ( set_input(In), Goal ), 
      set_input(SaveIn)
      ),
    close(In)
    ).

% asynchronously index all files under the specified roots,
% which should be an array of URIs for each root
start_index_roots(Roots) :-
  thread_create(index_roots(Roots), _Id, [detached(true)]).

index_roots(Roots) :-
  info("Starting indexing of files in all roots: %w", [Roots]),
  forall(member(Root, Roots), index_root(Root)),
  info("Finished indexing of files in all roots: %w", [Roots]).

index_root(URI) :-
  info("Starting index of files in root %w", [URI]),
  uri_file_name(URI, Directory),
  directory_source_files(Directory, Files, [recursive(true)]),
  forall(member(File, Files), index_file(File)),
  info("Finished index of files in root %w", [URI]).

index_file(Source) :-
  file_name_extension(_Base, Extension, Source),
  prolog_extension(Extension),
  uri_file_name(URI, Source),
  index_text(URI).

index_text(URI) :-
  uri_file_name(URI,FileName),
  xref_source(FileName),
  set_document_uri(URI), 
    !.

index_defined(URI, Defined, How) :-
  uri_file_name(URI, FileName),
  xref_defined(FileName, Defined, How).
  
prolog_extension(Extension) :-
  member(Extension, [
    pl,
    plt,
    pro,
    prolog
    ]).

  