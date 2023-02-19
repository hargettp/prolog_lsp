:- module(pls_index_indexing, [
  index_text/2
  ]).

:- use_module(library(option)).
:- use_module(documents).

% :- meta_predicate with_input_from(?,:).

% with_input_from(String, Module:Goal) :-
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

index_text(URI, Text) :-
  with_input_from(Text, 
    index_terms(URI)
    ).

index_terms(URI) :-
  read_in_term(URI, Term, Context),
  ( Term = end_of_file 
    -> true
    ; ( process_term(URI, Term, Context) , index_terms(URI))
    ).

read_in_term(URI, Term, Context) :-
  Context = [
    comments(_Comments),
    syntax_errors(dec10),
    variable_names(_VariableNames),
    term_position(_TermPosition)
    ],
  read_term(Term, Context),
  process_term(URI, Term, Context).

process_term(URI, :-(Directive), Context) :-
  process_directive(URI, Directive, Context).

process_term(URI, (Head :- Body), Context) :-
  process_clause(URI, Head, Body, Context).

process_term(URI, Fact, Context) :-
  process_clause(URI, Fact, Context).

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
  forall(arg(_I,Goal, Arg), once(process_directive_goal(URI, Arg, Context))).

process_directive_goal(_URI, Goal, _Context) :-
  var(Goal).

process_directive_goal(_URI, Goal, _Context) :-
  atom(Goal).
  
process_directive_goal(_URI, Goal, _Context) :-
  \+ compound(Goal).

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
  var(Goal).

process_goal(_URI, _Caller, Goal, _Context) :-
  atom(Goal).
  
process_goal(_URI, _Caller, Goal, _Context) :-
  \+ compound(Goal).

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
  