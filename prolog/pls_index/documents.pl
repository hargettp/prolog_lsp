:- module(pls_index_documents, [
  store_document/4,

  add_document_item/2,
  set_document_item/2,
  get_document_item/2,

  get_document_content/2
  ]).

:- dynamic document_item/2.
:- dynamic document_content/2.

store_document(URI, Language, Version, Content) :-
  retractall(document_item(URI, _)),
  set_document_item(URI, language(Language)),
  set_document_item(URI, version(Version)),
  set_document_content(URI, Content).

add_document_item(URI, Item) :-
  assertz(document_item(URI, Item)).

set_document_item(URI, Item) :-
  functor(Item, Name, Arity),
  functor(Clear, Name, Arity),
  clear_document_item(URI, Clear),
  assertz(document_item(URI, Item)).

get_document_item(URI, Item) :-
  document_item(URI, Item).

clear_document_items(URI) :-
  clear_document_item(URI, _).

clear_document_item(URI, Clear) :-
  retractall(document_item(URI, Clear)).

set_document_content(URI, Content) :-
  clear_document_content(URI),
  assertz(document_content(URI, Content)).

get_document_content(URI, Content) :-
  document_content(URI, Content).

clear_document_content(URI) :-
  retractall(document_content(URI, _)).

% 
% document accessors
% 

% -- content --
set_content(URI, Content) :-
  clear_language(URI),
  assertz(document_content(URI, Content)).

get_content(URI, Content) :-
  once(document_content(URI, Content)).

clear_content(URI) :-
  clear_document_content(URI).

% -- language --
set_language(URI, Language) :-
  clear_language(URI),
  set_document_item(URI, language(Language)).

get_language(URI, Language) :-
  once(get_document_item(URI, language(Language))).

clear_language(URI) :-
  clear_document_item(URI, language(_)).

% -- version --
set_version(URI, Version) :-
  clear_version(URI),
  set_document_item(URI, version(Version)).

get_version(URI, Version) :-
  once(get_document_item(URI, version(Version))).

clear_version(URI) :-
  clear_document_item(URI, version(_)).
