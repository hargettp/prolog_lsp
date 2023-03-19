:- module(pls_index_references, [
  references_for_position/3
]).

:- use_module(documents).

references_for_position(URI, Position, References) :-
  get_document_item(URI, Position, defines(Predicate)),
  get_references(Predicate, References).

references_for_position(URI, Position, References) :-
  get_document_item(URI, Position, references(_Caller, Predicate)),
  get_references(Predicate, References).

references_for_position(URI, Position, References) :-
  get_document_item(URI, Position, exports(Predicate)),
  get_references(Predicate, References).

get_references(Predicate, References) :-
  findall(
    _{
        uri: RefURI,
        range: RefRange
        },
    get_document_item(RefURI, RefRange, references(_RefCaller, Predicate)),
    References
  ).