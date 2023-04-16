:- module(pls_language_profile_base, [

]).

:- use_module(library(log4p)).
:- use_module(pls_index).

%! index_term(+URI, +Pos, +Term) is nondet.
%
% Indexing the term at the indicated Position in the
% source with the indicated URI.
%
pls_index_profiles:profile_index_term(base, URI, Pos, (:- module(Module, Exports))) :-
  term_position_range(URI, Pos, Range),
  pls_index:pls_index:add_document_item(URI, Range, module(Module, Exports)),
  % Arg of :-, which is the term position for module
  argument_positions(Pos, [DirectiveArgPos]),
  % Args of module: first is the name, second is export list
  argument_positions(DirectiveArgPos, [_,list_position(_,_,ExportPosList, _)]),
  index_exports(URI, Exports, ExportPosList),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- use_module(Module))) :-
  term_position_range(URI, Pos, Range),
  pls_index:add_document_item(URI, Range, uses(Module)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- reexport(Module))) :-
  term_position_range(URI, Pos, Range),
  pls_index:add_document_item(URI, Range, reexports(Module)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- reexport(Module, Imports))) :-
  term_position_range(URI, Pos, Range),
  pls_index:add_document_item(URI, Range, reexports(Module,Imports)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- [FileSpec])) :-
  term_position_range(URI, Pos, Range),
  pls_index:add_document_item(URI, Range, loads(FileSpec)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (:- include(FileSpec) )) :-
  term_position_range(URI, Pos, Range),
  pls_index:add_document_item(URI, Range, includes(FileSpec)),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (_Module:Head :- Body)) :-
  pls_index_profiles:profile_index_term(base, URI, Pos, (Head :- Body)).

pls_index_profiles:profile_index_term(base, URI, _Pos, :- provide_language_profile(Profile) ) :-
  register_language_profile(Profile, URI).

pls_index_profiles:profile_index_term(base, URI, Pos, :- Declaration ) :-
  Declaration =.. [Directive, Arg],
  member(Directive, [dynamic, multifile, discontiguous]),
  argument_positions(Pos, [ArgPos]),
  index_declaration(URI, ArgPos, Directive, Arg).

pls_index_profiles:profile_index_term(base, URI, Pos, (Head :- Body)) :-
  functor(Head, Name, Arity),
  Caller = Name/Arity,
  argument_positions(Pos, [HeadPos, BodyPos]),
  term_position_range(URI, HeadPos, Range),
  pls_index:add_document_item(URI, Range, defines(Caller)),
  index_goals(URI, Caller, BodyPos, Body),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, (Head --> Body)) :-
  functor(Head, Name, Arity),
  Caller = Name//Arity,
  argument_positions(Pos, [HeadPos, BodyPos]),
  term_position_range(URI, HeadPos, Range),
  pls_index:add_document_item(URI, Range, defines(Caller)),
  index_goals(URI, Caller, BodyPos, Body),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, _Module:Term) :-  
  pls_index_profiles:profile_index_term(base, URI, Pos, Term),
  !.

pls_index_profiles:profile_index_term(base, URI, Pos, Term) :-  
  functor(Term, Name, Arity),
  atom_codes(Name, [Initial|_]),
  code_type(Initial, prolog_atom_start),
  Caller = Name/Arity,
  term_position_range(URI, Pos, Range),
  pls_index:add_document_item(URI, Range, defines(Caller)),
  !.

pls_index_profiles:profile_index_docs(base, URI, SubPos, _Module:Head :- Body, CommentPos) :-
  pls_index_profiles:profile_index_docs(base, URI, SubPos, Head :- Body, CommentPos),
  !.

pls_index_profiles:profile_index_docs(base, URI, SubPos, Head :- _Body, CommentPos) :-
  term_position_range(URI, SubPos, Range),  
  functor(Head, Name, Arity),
  Predicate = Name/Arity,
  index_docs(URI, Predicate, Range, CommentPos),
  !.

pls_index_profiles:profile_index_docs(base, _URI, _SubPos, _Term, _CommentPos).

pls_index_profiles:profile_index_signature(base, URI, Pos, Head :- _Body, Vars) :-
  with_output_to(string(Signature),
    write_term(Head,[variable_names(Vars)])
    ),
  term_position_range(URI, Pos, Range),
  functor(Head, Name, Arity),
  pls_index:add_document_item(URI, Range, signature(Name/Arity, Signature)).

pls_index_profiles:profile_index_signature(base, _, _, _, _).

pls_index_profiles:profile_index_goal(base, URI, Caller, SubPos, _Module:Goal) :-
  argument_positions(SubPos, [_ModulePos, GoalPos]),
  index_goal(URI, Caller, GoalPos, Goal),
  !.

pls_index_profiles:profile_index_goal(base, URI, Caller, parentheses_term_position(_From, _To, ContentPos), Goal) :-
  index_goal(URI, Caller, ContentPos, Goal).

pls_index_profiles:profile_index_goal(base, URI, Caller, term_position(_From, _To, FFrom, FTo, _Subpos), Goal) :-
  functor_range(URI, FFrom, FTo, Range),
  ( Caller = _Name // _Arity
    -> ( functor(Goal, Name, Arity), Predicate = Name//Arity) 
    ; ( functor(Goal, Name, Arity), Predicate = Name/Arity)
    ),
  % Don't index references to this common fuctor, because its everywhere
  Name \= ',',
  Item = references(Caller, Predicate),
  debug("Adding item %w",[Item]),
  pls_index:add_document_item(URI, Range, Item) .

pls_index_profiles:profile_index_goal(base, URI, Caller, term_position(_From, _To, _FFrom, _FTo, Subpos), Goal) :-
  functor(Goal, _Name, Arity),
  between(1, Arity, Index),
  arg(Index, Goal, Arg),
  nth1(Index, Subpos, Pos),
  index_goal(URI, Caller, Pos, Arg).

pls_index_profiles:profile_end_of_file(base, _URI).  

pls_index_profiles:profile_symbol(base, URI, Query, Range, Name, Detail, Kind) :-
  get_document_item(URI, Range, defines(Predicate)),
  Predicate = Name/_Arity,
  term_string(Predicate, Detail),
  atom_concat(Query,_,Name),
  symbol_kind(function,Kind).

%! index_declaration(+URI, +Pos, +Directive, _Caller) is det.
%
% Index a declaration from a directive.
%
index_declaration(URI, Pos, Directive, (Predicate, MorePredicates)) :-
  argument_positions(Pos, [ArgPos, MoreArgsPos]),
  index_declaration(URI, ArgPos, Directive, Predicate),
  index_declaration(URI, MoreArgsPos, Directive, MorePredicates).

index_declaration(URI, Pos, _Directive, Predicate) :-
  argument_positions(Pos, [ArgPos]),
  term_position_range(URI, ArgPos, Range),
  pls_index:add_document_item(URI, Range, defines(Predicate)).

%! index_exports(+URI, +Exports, +ExportPosList) is nondet.
%
% Index the exports in a module declaration.
%
index_exports(_URI, [], []).

index_exports(URI, [Export | ExportRest], [ExportPos | ExportPosListRest]) :-
  term_position_range(URI, ExportPos, Range),
  pls_index:add_document_item(URI, Range, exports(Export)),
  index_exports(URI, ExportRest, ExportPosListRest).