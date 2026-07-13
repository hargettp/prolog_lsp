:- module(lsp_logs, [
   use_log_file_handler/1
]).

:- use_module(library(log4p)).
:- use_module(library(yall)).

log4p:timestamped_log_handler(Level, Message) :-
   get_time(Timestamp),
   format_time(string(FormattedTimestamp), "%FT%T%z", Timestamp),
   process_id(PID),
   ignore(
     (writef('%w : %w : %w : %w\n',[FormattedTimestamp, Level, PID, Message]),
       flush_output)
   ).

use_log_file_handler(FileName) :-
   retractall(log4p:log_handler(_)),
   open(FileName, append, LogStream,[create([default])]),  
   LogHandler = {LogStream}/[Level, Message] >>
     with_output_to(LogStream, timestamped_log_handler(Level,Message)),
   add_log_handler(LogHandler),
   at_halt(close(LogStream)).