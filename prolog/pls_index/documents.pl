:- module(pls_index_documents, [
  store_document/4,

  get_document_properties/2,
  add_document_property/2,
  set_document_property/2,
  get_document_property/2,
  clear_document_properties/1,

  get_document_items/2,
  add_document_item/2,
  set_document_item/2,
  get_document_item/2,
  clear_document_items/1,

  get_document_content/2
  ]).

:- dynamic document_property/2.
:- dynamic document_item/2.
:- dynamic document_content/2.

store_document(URI, Language, Version, Content) :-
  clear_document_properties(URI),
  set_document_property(URI, language(Language)),
  set_document_property(URI, version(Version)),
  set_document_content(URI, Content).

get_document_properties(URI, Properties) :-
  findall(Property, document_property(URI, Property), Properties).

add_document_property(URI, Property) :-
  assertz(document_property(URI, Property)).

set_document_property(URI, Property) :-
  functor(Property, Name, Arity),
  functor(Clear, Name, Arity),
  clear_document_property(URI, Clear),
  assertz(document_property(URI, Property)).

get_document_property(URI, Property) :-
  document_property(URI, Property).

clear_document_properties(URI) :-
  clear_document_property(URI, _).

clear_document_property(URI, Clear) :-
  retractall(document_property(URI, Clear)).

get_document_items(URI, Items) :-
  findall(Item, document_item(URI, Item), Items).

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
  set_document_property(URI, language(Language)).

get_language(URI, Language) :-
  once(get_document_property(URI, language(Language))).

clear_language(URI) :-
  clear_document_property(URI, language(_)).

% -- version --
set_version(URI, Version) :-
  clear_version(URI),
  set_document_property(URI, version(Version)).

get_version(URI, Version) :-
  once(get_document_property(URI, version(Version))).

clear_version(URI) :-
  clear_document_property(URI, version(_)).
