:- module(pls_index_defintions, [
  definition_for_position/3
]).

:- use_module(library(log4p)).
:- use_module(documents).

definition_for_position(URI, Position, Definitions) :-
  get_document_item(URI, Position, exports(Callable)),
  get_definitions(Callable, Definitions).

definition_for_position(URI, Position, Definitions) :-
  get_document_item(URI, Position, references(_Caller, Callable)),
  get_definitions(Callable, Definitions).

get_definitions(Callable, Definitions) :-
  findall(
    _{
      uri: DefURI,
      range: DefRange
      }, 
    get_document_item(DefURI, DefRange, defines(Callable)), 
    Definitions
    ).