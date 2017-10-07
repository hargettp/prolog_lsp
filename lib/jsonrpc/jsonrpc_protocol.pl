:- module(json_protocol, [
  read_message/3,
  write_message/2
  ]).

:- use_module(library(log4p)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

read_message(In,Size,Message) :-
    read_string(In,Size,String),
    (String = "" ->
      throw(eof) ;
      ( atom_string(Atom,String),
      atom_json_dict(Atom,Message,[])) ).

write_message(Out,Message) :-
  json_write(Out,Message),
  write(Out,'\n'),
  flush_output(Out).
