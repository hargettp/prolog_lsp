:- module(pls_symbols, [
  workspace_symbols/2,
  document_symbols/2
  ]).

:- use_module(library(uri)).

:- use_module(files).
:- use_module(pls_index).

workspace_symbols(Query,Symbols) :-
  findall(Symbol,workspace_symbol(Query,Symbol),Symbols).

workspace_symbol(Query,Symbol) :-
  get_document_item(URI, Range, defines(Callable)),
  Callable = Name/_Arity,
  atom_concat(Query,_,Name),
  symbol_kind(function,Kind),
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
  get_document_item(URI, Range, defines(Callable)),
  Callable = Name/_Arity,
  symbol_kind(function,Kind),
  term_string(Callable, Detail),
  Symbol = symbol{
    name: Name,
    detail: Detail,
    kind: Kind,
    range: Range,
    selectionRange: Range
    }.

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
