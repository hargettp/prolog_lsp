:- module(pls_index_indexing, [
  index_text/1,

  begin_indexing/1,
  start_index_roots/1,

  index_roots/1,
  index_root/1
  ]).

:- use_module(library(log4p)).
:- use_module(library(option)).
:- use_module(library(prolog_source)).
:- use_module(library(prolog_stack)).
:- use_module(library(uri)).

:- use_module(documents).
:- use_module(lines).
:- use_module(terms).

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

%! begin_indexing(+Params) is det.
%
%  Regardless of params, we always succeed
%
begin_indexing(Params) :-
  RootURI = Params.get(rootUri),
  (Folders = Params.get(workspaceFolders,[])
    -> true
    % if we are on the false path, we assume it was null instead
    ; Folders = []
    ),
  findall(
    URI,
    ( member(Folder, Folders), URI = Folder.uri ),
    FolderURIs
    ),
    swi_root(SwiRootURI),
    pack_roots(PackRoots),
    append([
      [RootURI],
      [SwiRootURI],
      PackRoots,
      FolderURIs
      ],
      AllRoots),
    % list_to_set([RootURI | [SwiRootURI |FolderURIs]], RootURIs),
    list_to_set(AllRoots, RootURIs),
    start_index_roots(RootURIs).

begin_indexing(_).

%! start_index_roots(+Roots) is det.
%
% Asynchronously index all files under the specified roots,
% which should be an array of URIs for each root
%
start_index_roots(Roots) :-
  thread_create(index_roots(Roots), _Id, [detached(true)]).

%! index_roots(+Roots) is det.
%
% Given a list of root URIs, index all of the prolog source underneath
% each root.
% 
index_roots(Roots) :-
  debug("Starting indexing of files in all roots: %w", [Roots]),
  forall(member(Root, Roots), index_root(Root)),
  debug("Finished indexing of files in all roots: %w", [Roots]).

%! index_root(+URI) is det.
%
% Index all of the source files under the indicated root.
%
index_root(URI) :-
  debug("Starting index of files in root %w", [URI]),
  uri_file_name(URI, Directory),
  directory_source_files(Directory, Files, [recursive(true)]),
  forall(member(File, Files), index_file(File)),
  debug("Finished index of files in root %w", [URI]).

%! index_file(+Source) is det.
%
% Index the specified source file.
%
index_file(Source) :-
  debug("Starting index of file %w", [Source]),
  file_name_extension(_Base, Extension, Source),
  prolog_extension(Extension),
  uri_file_name(URI, Source),
  catch_with_backtrace(
    index_text(URI),
    Error,
    error(Error)
    ),
  debug("Finished index of file %w", [Source]).

%! index_text(+URI) is det.
%
% Given a URI for a source file, index its text, taking into account
% whether there is in-memory data for an open file or the current content
% on the filesystem.
%
index_text(URI) :-
  uri_file_name(URI,FileName),
  set_document_uri(URI), 
  xref_source(FileName),
  index_lines(URI),
  index_terms(URI),
  !.

%! swi_root(-SwiRootURI) is nondet.
%
% Return the root for the SWI-Prolog library.
%
swi_root(SwiRootURI) :-
  absolute_file_name(swi(library),SwiRoot, [file_type(directory)]),
  uri_file_name(SwiRootURI, SwiRoot).

%! pack_roots(-PackRoots) is det.
%
% Return the roots for each installed package.
%
pack_roots(PackRoots) :-
  findall(
    PackRoot,
    (
      pack_property(_Pack, directory(PackDirectory)),
      directory_file_path(PackDirectory,'prolog',PackPrologDirectory),
      uri_file_name(PackRoot, PackPrologDirectory)
      ),
    PackRoots
    ).
  
prolog_extension(Extension) :-
  member(Extension, [
    pl,
    plt,
    pro,
    prolog
    ]).

