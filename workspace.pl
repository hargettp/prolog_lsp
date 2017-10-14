:- module(workspace, [
  workspace_source_file/2,
  workspace_file/2,
  workspace_file_type/2
  ]).

:- use_module(library(uri)).

:- use_module(files).

workspace_source_file(RootUri,SourceUri) :-
  workspace_file(RootUri,SourceUri),
  workspace_file_type(source,SourceUri).

workspace_file(RootUri,FileUri) :-
  uri_file_name(RootUri,Root),
  directory_file(Root,File),
  uri_file_name(FileUri,File).

workspace_file_type(source,FileUri) :-
  uri_file_name(FileUri,File),
  file_name_extension(_Base,Extension,File),
  member(Extension,[pl]).

workspace_file_type(test,FileUri) :-
  uri_file_name(FileUri,File),
  file_name_extension(_Base,Extension,File),
  member(Extension,[plt]),!.

workspace_file_type(doc,FileUri) :-
  uri_file_name(FileUri,File),
  file_name_extension(_Base,Extension,File),
  member(Extension,[txt,md]),!.

workspace_file_type(doc,FileUri) :-
  uri_file_name(FileUri,File),
  file_name_extension(_Base,Extension,File),
  member(Extension,[txt,md]),!.

workspace_file_type(other,_).
