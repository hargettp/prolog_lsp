:- module(workspace, [
  workspace_source_file/2,
  workspace_file/2,
  workspace_file_type/2,

  workspace_symbol/2,
  workspace_symbols/2,

  workspace_symbol_info/2,
  workspace_symbol_infos/2
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

workspace_file_type(other,_).

workspace_symbol(_Query,Symbol) :-
  xref_defined(_,Callable,local(_)),
  Callable =.. [Symbol|_].

workspace_symbol_info(_Query,SymbolInfo) :-
  xref_defined(File,Callable,local(StartLine)),
  uri_file_name(FileUri,File),
  Callable =.. [Symbol|_],
  EndLine is StartLine + 1,
  symbol_kind(function,Kind),
  module_file(Module,File),
  SymbolInfo = symbol_info{
    name: Symbol,
    kind: Kind,
    location: {
      uri: FileUri,
      range: range{
        start: position{line: StartLine, character: 0},
        end: position{line: EndLine, character: 0}
        }
      },
    container: Module
    }.

workspace_symbol_infos(Query,SymbolInfos) :-
  findall(SymbolInfo,workspace_symbol_info(Query,SymbolInfo),SymbolInfos).

workspace_symbols(Query,Symbols) :-
  findall(Symbol,workspace_symbol(Query,Symbol),RawSymbols),
  sort(RawSymbols,Symbols).

module_file(Module,File) :-
  file_base_name(File,Name),
  file_name_extension(Module,pl,Name).

symbol_kind(file, 1).
symbol_kind(module, 2).
symbol_kind(namespace, 3).
symbol_kind(package, 4).
symbol_kind(class, 5).
symbol_kind(method, 6).
symbol_kind(property, 7).
symbol_kind(field, 8).
symbol_kind(constructor, 9).
symbol_kind(enum, 10).
symbol_kind(interface, 11).
symbol_kind(function, 12).
symbol_kind(variable, 13).
symbol_kind(constant, 14).
symbol_kind(string, 15).
symbol_kind(number, 16).
symbol_kind(boolean, 17).
symbol_kind(array, 18).
