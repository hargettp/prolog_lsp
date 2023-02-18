:- module(pls_index_indexing, [
  index_text/2
  ]).

:- use_module(library(option)).
:- use_module(documents).

with_input_from(String, Module:Goal) :-
  setup_and_call_cleanup(
    open_string(String, In),
    setup_and_call_cleanup(
      ( current_input(SaveIn), set_input(In) ),
      Module:Goal, 
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
    syntax_errors(_SyntaxErrors),
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

process_clause(URI, Head, Body, Context) :-
  process_clause(URI, Head, Context),
  process_body(URI, Head, Body, Context).

process_clause(URI, Fact, Context) :-
  functor(Fact, Name, Arity),
  option(term_postion(Position), Context),
  set_document_item(URI, declares(Name/Arity, Position)),
  ( option(comments(Docs), Context) 
    -> set_document_item(URI, docs(Name/Arity, Docs))
    ; true
    ).

process_body(_URI, _Head, _Body, _Context).
  