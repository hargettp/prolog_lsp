:- module(pls_index_indexing, [
  index_text/1,

  start_index_roots/1,

  index_roots/1,
  index_root/1,

  index_defined/3
  ]).

:- use_module(library(lambda_abstractions)).
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

% 
% helpers
% 

% Given a document URI, successive calls will iterate
% over *all* the compound and atomic subterms inside
% the document; this is the basis for indexing
% calls and other cross-references
document_subterm(URI, Term, From, To, Subterm) :-
  get_document_content(URI, Content),
  !,
  setup_call_cleanup(
    open_string(Content, In),
    read_subterm(In, Term, From, To, Subterm),
    close(In)
    ).

document_subterm(URI, Term, From, To, Subterm) :-
  uri_file_name(URI, FileName),
  setup_call_cleanup(
    open(FileName, read, In),
    read_subterm(In, Term, From, To, Subterm),
    close(In)
    ).

read_subterm(In, Term, From, To, Subterm) :-
  repeat,
  read_term(In, Term, [subterm_positions(Pos)]),
  ( Term \== end_of_file
    -> find_subterm(In, Pos, From, To, Subterm)
    ; (!, fail)
    ).
  
% primitives: atoms, numbers, booleans
find_subterm(In, From-To, From, To, Subterm) :-
  fetch_from_stream(In, From, To, Text),
  term_string(Subterm, Text),
  \+ number(Subterm),
  \+ var(Subterm).

% strings
find_subterm(In, string_position(From, To), From, To, Subterm) :-
  fetch_from_stream(In, From, To, Subterm).

% brace expressions
find_subterm(In, brace_term_position(_From, _To, Arg), From, To, Subterm) :-
  % fetch_from_stream(In, From, To, Subterm),
  find_subterm(In, Arg, From, To, Subterm).

% lists
find_subterm(In, list_position(_From, _To, Elements, _Tail), From, To, Subterm) :-
  member(Element, Elements),
  find_subterm(In, Element, From, To, Subterm).

% compound terms
find_subterm(In, term_position(From, To, _FFrom, _FTo, _Args), From, To, Subterm) :-
  fetch_from_stream(In, From, To, Text),
  term_string(Subterm, Text).

find_subterm(In, term_position(_From, _To, _FFrom, _FTo, Args), From, To, Subterm) :-
  member(Arg, Args),
  find_subterm(In, Arg, From, To, Subterm).

% dictionaries
find_subterm(In, dict_position(_From, _To, _TagFrom, _TagTo, KeyValuePosList), From, To, Subterm) :-
  member(KeyValuePos, KeyValuePosList),
  find_subterm(In, KeyValuePos, From, To, Subterm).

% dictionary key/value pair
find_subterm(In, key_value_position(_From, _To, _SepFrom, _SepTo, _Key, KeyPos, ValuePos), From, To, Subterm) :-
  % fetch_from_stream(In, From, To, Subterm).
  ( find_subterm(In, KeyPos, From, To, Subterm) ; find_subterm(In, ValuePos, From, To, Subterm) ).

% terms between parentheses
find_subterm(In, parentheses_term_position(_From, _To, ContentPos), From, To, Subterm) :-
  % fetch_from_stream(In, From, To, Subterm).
  find_subterm(In, ContentPos, From, To, Subterm).

fetch_from_stream(In, From, To, Value) :-
  Length is To - From,
  setup_and_call_cleanup(
    stream_property(In, position(Current)),
    (seek(In, From, bof, _), read_string(In, Length, Value) ),
    set_stream_position(In, Current)
  ).

document_line_positions(URI, LinePositions) :-
  findall(Line-Position,document_line_position(URI, Line, Position), LinePositions).

document_line_position(URI, Line, Position) :-
  get_document_content(URI, Content),
  !,
  setup_call_cleanup(
    open_string(Content, In),
    stream_line_position(In, Line, Position),
    close(In)
    ).

document_line_position(URI, Line, Position) :-
  uri_file_name(URI, FileName),
  file_line_position(FileName, Line, Position).

file_line_position(FileName, Line, Position) :-
  setup_call_cleanup(
    open(FileName, read, In),
    stream_line_position(In, Line, Position),
    close(In)
    ).

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
  