:- module(pls_index_defintions, [
  definition_for_position/3
]).

:- use_module(library(log4p)).
:- use_module(documents).

definition_for_position(URI, Position, Definitions) :-
  get_document_item(URI, Position, exports(Predicate)),
  get_definitions(Predicate, Definitions).

definition_for_position(URI, Position, Definitions) :-
  get_document_item(URI, Position, references(_Caller, Predicate)),
  get_definitions(Predicate, Definitions).

get_definitions(Predicate, Definitions) :-
  findall(
    _{
      uri: DefURI,
      range: DefRange
      }, 
    get_document_item(DefURI, DefRange, defines(Predicate)),
    Definitions
    ).