:- module(pls_index_indexing, [
  index_text/2,

  start_index_roots/1,

  index_roots/1,
  index_root/1
  ]).

:- use_module(library(log4p)).
:- use_module(library(option)).
:- use_module(library(prolog_source)).
:- use_module(library(uri)).

:- use_module(documents).

with_input_from(String, Goal) :-
  setup_and_call_cleanup(
    open_string(String, In),
    setup_and_call_cleanup(
      current_input(SaveIn),
      ( set_input(In), Goal ), 
      set_input(SaveIn)
      ),
    close(In)
    ).

% asynchronously index all files under the specified roots,
% which should be an array of URIs for each root
start_index_roots(Roots) :-
  thread_create(index_roots(Roots), _Id, [detached(true)]).

index_roots(Roots) :-
  info("Starting indexing of files in all roots: %w", [Roots]),
  forall(member(Root, Roots), index_root(Root)),
  info("Finished indexing of files in all roots: %w", [Roots]).

index_root(URI) :-
  info("Starting index of files in root %w", [URI]),
  uri_file_name(URI, Directory),
  directory_source_files(Directory, Files, [recursive(true)]),
  forall(member(File, Files), index_file(File)),
  info("Finished index of files in root %w", [URI]).

index_file(Source) :-
  file_name_extension(_Base, Extension, Source),
  prolog_extension(Extension),
  read_file_to_string(Source, Content, []),
  uri_file_name(URI, Source),
  index_text(URI, Content).

index_text(URI, Text) :-
  clear_document_items(URI),
  with_input_from(Text, 
    index_terms(URI)
    ), !.

index_terms(URI) :-
  read_in_term(Term, Context),
  process_term(URI, Term, Context),
  ( Term = end_of_file 
    -> true
    ; index_terms(URI)
    ).

read_in_term(Term, Context) :-
  Context = [
    comments(_Comments),
    syntax_errors(dec10),
    variable_names(_VariableNames),
    term_position(_TermPosition)
    ],
  read_term(Term, Context).

process_term(_URI, end_of_file, _).

process_term(URI, :-(Directive), Context) :-
  process_directive(URI, Directive, Context).

process_term(URI, (Head :- Body), Context) :-
  process_clause(URI, Head, Body, Context).

process_term(URI, Fact, Context) :-
  process_clause(URI, Fact, Context).

process_directive(URI, [FileSpec], Context) :-
  option(term_position(Position), Context),
  add_document_item(URI, loads(FileSpec, Position)).

process_directive(URI, module(Name, PublicList), _Context) :-
  set_document_item(URI, module(Name)),
  set_document_item(URI, exports(PublicList)).

process_directive(URI, use_module(Module), Context) :-
  process_directive(URI, use_module(Module, []), Context).

process_directive(URI, use_module(Module, ImportList), _Context) :-
  add_document_item(URI, uses_module(Module, ImportList)).

process_directive(URI, Goal, Context) :-
  functor(Goal, Name, Arity),
  ( is_builtin(Name/Arity)
    -> true
    ; process_directive_call(URI, Name/Arity, Context)
    ),
  (Arity > 0 
    -> forall(arg(_I,Goal, Arg), once(process_directive_goal(URI, Arg, Context)))
    ; true).

process_directive_goal(_URI, Goal, _Context) :-
  var(Goal), !.

process_directive_goal(_URI, Goal, _Context) :-
  atom(Goal), !.
  
process_directive_goal(_URI, Goal, _Context) :-
  \+ compound(Goal), !.

process_directive_goal(URI, Goal, Context) :-
  functor(Goal, Name, Arity),
  ( is_builtin(Name/Arity)
    -> true
    ; process_directive_call(URI, Name/Arity, Context)
    ),
  ( compound(Goal) 
    -> forall(arg(_I,Goal, Arg), once(process_directive_goal(URI, Arg, Context)))
    ; true
    ).

process_directive_goal(URI, Goal, Context) :-
  functor(Goal, Name, Arity),
  process_directive_call(URI, Name/Arity, Context).

process_directive_call(URI, Name/Arity, Context) :-
  option(term_position(Position), Context),
  add_document_item(URI, calls(Name/Arity, Position)).

process_clause(URI, Head, Body, Context) :-
  process_clause(URI, Head, Context),
  process_body(URI, Head, Body, Context).

process_clause(URI, Fact, Context) :-
  functor(Fact, Name, Arity),
  option(term_position(Position), Context),
  set_document_item(URI, declares(Name/Arity, Position)),
  option(comments(Docs), Context),
  set_document_item(URI, docs(Name/Arity, Docs)).

process_body(URI, Head, Body, Context) :-
  functor(Head, Name, Arity),
  process_goal(URI, Name/Arity, Body, Context).

process_goal(_URI, _Caller, Goal, _Context) :-
  var(Goal), !.

process_goal(_URI, _Caller, Goal, _Context) :-
  atom(Goal), !.
  
process_goal(_URI, _Caller, Goal, _Context) :-
  \+ compound(Goal), !.

process_goal(URI, CallerName/CallerArity, Goal, Context) :-
  functor(Goal, Name, Arity),
  ( is_builtin(Name/Arity)
    -> true
    ; process_call(URI, CallerName/CallerArity, Name/Arity, Context)
    ),
  forall(arg(_I,Goal, Arg), once(process_goal(URI, CallerName/CallerArity, Arg, Context))).

process_goal(URI, CallerName/CallerArity, Goal, Context) :-
  functor(Goal, Name, Arity),
  process_call(URI, CallerName/CallerArity, Name/Arity, Context).

process_call(URI, CallerName/CallerArity, Name/Arity, Context) :-
  option(term_position(Position), Context),
  add_document_item(URI, calls(CallerName/CallerArity, Name/Arity, Position)).

is_builtin(Name/Arity) :-
  member(Name/Arity, [
    (',')/2,
    ('[|]')/2,
    (;)/2,
    (->)/2,
    (\+)/1,
    ('.')/3
    ]).
  
  prolog_extension(Extension) :-
    member(Extension, [
      pl,
      plt,
      pro,
      prolog
      ]).