:- module(pls_symbols, [
  workspace_symbols/2,
  document_symbols/2
  ]).

:- use_module(library(uri)).

:- use_module(pls_index).

workspace_symbols(Query,Symbols) :-
  findall(Symbol,workspace_symbol(Query,Symbol),Symbols).

workspace_symbol(Query,Symbol) :-
  try_profile_symbol(URI, Query, Range, Name, _Detail, Kind),
  Symbol = symbol{
    name: Name,
    kind: Kind,
    location: _{
      uri: URI,
      range: Range
      }
    }.

document_symbols(URI, SymbolInfos) :-
  findall(SymbolInfo, document_symbol(URI, SymbolInfo), SymbolInfos).

document_symbol(URI, Symbol) :-
  try_profile_symbol(URI, '', Range, Name, Detail, Kind),
  Symbol = symbol{
    name: Name,
    detail: Detail,
    kind: Kind,
    range: Range,
    selectionRange: Range
    }.

try_profile_symbol(URI, Query, Range, Name, Detail, Kind) :-
  get_document_profile(URI, Profile),
  Profile \= base,
  pls_index_profiles:profile_symbol(Profile, URI, Query, Range, Name, Detail, Kind).

try_profile_symbol(URI, Query, Range, Name, Detail, Kind) :-
  pls_index_profiles:profile_symbol(base, URI, Query, Range, Name, Detail, Kind).
