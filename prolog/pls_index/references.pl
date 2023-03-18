:- module(pls_index_references, [
  references_for_position/3
]).

:- use_module(documents).

references_for_position(URI, Position, References) :-
  get_document_item(URI, Position, defines(Callable)),
  get_references(Callable, References).

references_for_position(URI, Position, References) :-
  get_document_item(URI, Position, references(_Caller, Callable)),
  get_references(Callable, References).

references_for_position(URI, Position, References) :-
  get_document_item(URI, Position, exports(Callable)),
  get_references(Callable, References).

get_references(Callable, References) :-
  findall(
    _{
        uri: RefURI,
        range: RefRange
        },
    get_document_item(RefURI, RefRange, references(_RefCaller, Callable)),
    References
  ).