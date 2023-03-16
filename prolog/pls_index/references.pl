:- module(pls_index_references, [
  references_for_position/3
]).

:- use_module(documents).

references_for_position(URI, Position, References) :-
  findall(
    Reference, 
    ( 
      definition_reference(URI, Position, Reference) ; 
      call_reference(URI, Position, Reference) 
      ), 
    References
    ).

definition_reference(URI, Position, Reference) :-
  get_document_item(URI, Position, defines(Callable)),
  get_document_item(RefURI, RefRange, references(_Caller, Callable)),
  Reference = _{
    uri: RefURI,
    range: RefRange
    }.

call_reference(URI, Position, Reference) :-
  get_document_item(URI, Position, references(_Caller, Callable)),
  get_document_item(RefURI, RefRange, references(_RefCaller, Callable)),
  Reference = _{
    uri: RefURI,
    range: RefRange
    }.
