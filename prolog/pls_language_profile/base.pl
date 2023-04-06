:- module(pls_language_profile_base, [

]).

:- use_module('../pls_index').

%! index_term(+URI, +Pos, +Term) is nondet.
%
% Indexing the term at the indicated Position in the
% source with the indicated URI.
%
pls_index_profiles:profile_index_term(base, URI, Pos, (:- module(Module, Exports))) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, module(Module, Exports)),
  % Arg of :-, which is the term position for module
  argument_positions(Pos, [DirectiveArgPos]),
  % Args of module: first is the name, second is export list
  argument_positions(DirectiveArgPos, [_,list_position(_,_,ExportPosList, _)]),
  index_exports(URI, Exports, ExportPosList),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- use_module(Module))) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, uses(Module)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- reexport(Module))) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, reexports(Module)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- reexport(Module, Imports))) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, reexports(Module,Imports)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- [FileSpec])) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, loads(FileSpec)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- include(FileSpec) )) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, includes(FileSpec)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (_Module:Head :- Body)) :-
  index_term(URI, Pos, (Head :- Body)).

pls_index_profiles:profile_index_term(base, URI, Pos, (Head :- Body)) :-
  functor(Head, Name, Arity),
  Caller = Name/Arity,
  argument_positions(Pos, [HeadPos, BodyPos]),
  term_position_range(URI, HeadPos, Range),
  add_document_item(URI, Range, defines(Caller)),
  index_goals(URI, Caller, BodyPos, Body),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (Head --> Body)) :-
  functor(Head, Name, Arity),
  Caller = Name//Arity,
  argument_positions(Pos, [HeadPos, BodyPos]),
  term_position_range(URI, HeadPos, Range),
  add_document_item(URI, Range, defines(Caller)),
  index_goals(URI, Caller, BodyPos, Body),
  !.

%! index_comments(+URI, +CommentPos, +TermPos) is nondet.
%
% Index the documentation for the term at the indicated TermPos,
% using the CommentPos from an earlier `read_term/3` call.
%
pls_index_profiles:profile_index_comments(base, URI, SubPos, Term, CommentPos) :-
  term_position_range(URI, SubPos, Range),
  index_docs(URI, Term, Range, CommentPos),
  !.

pls_index_profiles:profile_index_signature(base, URI, Pos, Head :- _Body, Vars) :-
  with_output_to(string(Signature),
    write_term(Head,[variable_names(Vars)])
    ),
  term_position_range(URI, Pos, Range),
  functor(Head, Name, Arity),
  add_document_item(URI, Range, signature(Name/Arity, Signature)).

pls_index_profiles:profile_index_signature(base, _, _, _, _).

%! index_exports(+URI, +Exports, +ExportPosList) is nondet.
%
% Index the exports in a module declaration.
%
index_exports(_URI, [], []).

index_exports(URI, [Export | ExportRest], [ExportPos | ExportPosListRest]) :-
  term_position_range(URI, ExportPos, Range),
  add_document_item(URI, Range, exports(Export)),
  index_exports(URI, ExportRest, ExportPosListRest).