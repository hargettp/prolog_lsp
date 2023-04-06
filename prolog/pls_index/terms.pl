:- module(pls_index_terms, [
  index_terms/1
  ]).

:- use_module(library(http/json)).
:- use_module(library(log4p)).
:- use_module(library(prolog_stack)).

:- use_module(documents).
:- use_module(docs).
:- use_module(profiles).

% Index the terms in a file, including subterms.
% A file or document is a sequence of terms, and
% terms contains goals.
index_terms(URI) :-
  clear_document_items(URI),
  forall(index_term(URI), true).

%! index_term(+URI) is nondet.
%
% On succesive calls, read and index every time in the source at URI.
%
index_term(URI) :-
  with_content(URI, In, (
    repeat,
    read_term(In, Term, [
      syntax_errors(dec10),
      subterm_positions(SubPos), 
      comments(CommentPos),
      variable_names(Vars)
      ]),
    ( Term \== end_of_file
      -> process_term(URI, SubPos, Term, CommentPos, Vars)
      ; (!, fail)
      )
    )).

process_term(URI, _SubPos, (:- use_language_profile(Profile)), _CommentPos, _Vars) :-
  set_document_profile(URI, Profile),
  !.

process_term(URI, SubPos, Term, CommentPos, Vars) :-
  get_document_profile(URI, Profile),
  ensure_profile_loaded(Profile),
  pls_index_profiles:profile_index_term(Profile, URI, SubPos, Term),
  pls_index_profiles:profile_index_comments(Profile, URI, SubPos, Term, CommentPos),
  pls_index_profiles:profile_index_signature(Profile, URI, SubPos, Term, Vars),
  !.
