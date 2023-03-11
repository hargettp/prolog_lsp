:- module(pls_index_indexing, [
  index_text/1,

  start_index_roots/1,

  index_roots/1,
  index_root/1,

  index_defined/3,
  index_lines/1
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

index_lines(URI) :-
  clear_document_lines(URI),
  clear_document_line_count(URI),
  forall(
    document_line_position(URI, Line, Position), 
    add_document_line(URI, Line, Position)
    ),
  findall(Line, get_document_line_position(URI, Line, _), Lines),
  max_list(Lines, LineCount),
  set_document_line_count(URI, LineCount).

document_line_position(URI, Line, Position) :-
  with_content(URI, In, 
    stream_line_position(In, Line, Position)
    ).
%   get_document_content(URI, Content),
%   !,
%   setup_call_cleanup(
%     open_string(Content, In),
%     stream_line_position(In, Line, Position),
%     close(In)
%     ).

% document_line_position(URI, Line, Position) :-
%   uri_file_name(URI, FileName),
%   setup_call_cleanup(
%     open(FileName, read, In),
%     stream_line_position(In, Line, Position),
%     close(In)
%     ).

stream_line_position(_In, 1, 0).
stream_line_position(In, Line, Position) :-
  repeat,
  read_line_to_string(In, Text),
  ( Text \== end_of_file
    -> (
        line_count(In, Line),
        character_count(In, Position)
      )
    ; ( !, fail)
    ).
  