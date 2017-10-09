:- module(json_protocol, [
  read_header/2,
  read_message/3,
  write_message/2
  ]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

:- use_module(library(log4p)).

read_message(In,Size,Message) :-
    read_string(In,Size,String),
    (String = "" ->
      throw(eof) ;
      (
        atom_string(Atom,String),
        catch(
          atom_json_dict(Atom,Message,[]),
          error(syntax_error(json(illegal_json)),_),
          fail
          )
      ) ).

write_message(Out,Message) :-
  with_output_to(string(RawMessage),json_write(current_output,Message)),
  atom_length(RawMessage,ContentLength),
  format(Out,"Content-Length: ~d\r\n\r\n",[ContentLength]),
  write(Out,RawMessage),
  write(Out,'\r\n'),
  flush_output(Out).

read_header(In, Size) :-
  read_content_length(In,Size),
  read_content_type(In),
  read_blank_line(In).

read_content_length(In,Size) :-
  stream_property(In,position(Pos)),
  read_line_to_codes(In,Codes),
  ( phrase(content_length(Size),Codes,[]) ;
    ( set_stream_position(In,Pos), Size = 0 )).

read_content_type(In) :-
  stream_property(In,position(Pos)),
  read_line_to_codes(In,Codes),
  ( phrase(content_type,Codes,[]) ;
    ( set_stream_position(In,Pos), true )).

read_blank_line(In) :-
  read_line_to_codes(In,Codes),
  phrase(blank_line,Codes,[]).

content_length(Size) -->
  "Content-Length",
  whites,
  ":",
  whites,
  digits(Digits),
  { string_codes(String, Digits), number_string(Size,String)}.

content_type -->
  "Content-Type",
  whites,
  ":",
  remainder(_).

blank_line -->
  whites.
