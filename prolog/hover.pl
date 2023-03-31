:- module(pls_index_hover, [
  hover_for_position/3
]).

:- use_module(pls_index).

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, exports(Predicate)),
  get_document_item(URI, Range, exports(Predicate)),
  get_hover(Predicate, Range, Hover),
  !.

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, references(Caller, Predicate)),
  get_document_item(URI, Range, references(Caller, Predicate)),
  get_hover(Predicate, Range, Hover),
  !.

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, defines(Predicate)),
  get_document_item(URI, Range, defines(Predicate)),
  get_hover(Predicate, Range, Hover),
  !.

%! get_hover(+Callablle, +Range, -Hover) is nondet.
%
% Return a hover for the indicated range; will always
% return minimal markup, even if no documentation available.
%
get_hover(Predicate, Range, Hover) :-
  get_docs(Predicate, Docs),
  Hover = _{
    range: Range,
    contents: _{
      kind: 'markdown',
      value: Docs
      }
  }.
