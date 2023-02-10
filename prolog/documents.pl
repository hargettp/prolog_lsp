:- module(lsp_documents, [
  store_document/4,
  find_document_item/3,
  find_document_content/2
  ]).

:- dynamic document_item/3.
:- dynamic document_content/2.

store_document(URI, Language, Version, Content) :-
  retractall(document_item(URI, _, _)),
  assertz(document_item(URI, Language, Version)),
  retractall(document_content(URI, _)),
  assertz(document_content(URI, Content)).

find_document_item(URI, Language, Version) :-
  document_item(URI, Language, Version).

find_document_content(URI, Content) :-
  document_content(URI, Content).
