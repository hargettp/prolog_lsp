:- module(pls_index_defintions, [
  definition_for_position/3
]).

:- use_module(library(log4p)).
:- use_module(documents).

definition_for_position(URI, Position, References) :-
  get_document_item(URI, Position, references(_Caller, Callable)),
  info("Found %w for callable %w",[Position, Callable]),
  findall(
    _{
      uri: DefURI,
      range: DefRange
      }, 
    get_document_item(DefURI, DefRange, defines(Callable)), 
    References
    ).
