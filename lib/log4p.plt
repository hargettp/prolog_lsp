:- begin_tests(log4p).

:- use_module(log4p).

test(default_log_level) :-
  log_level(info).

test(set_log_level) :-
  log_level(info),
  set_log_level(warn,info),
  log_level(warn),
  set_log_level(info,warn),
  log_level(info),
  findall(Level,log_level(Level),[info]).

test(log_levels) :-
  log_levels([trace,debug,info,warn,error,fatal]).

test(valid_default_log_levels) :-
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[info,warn,error,fatal]]).

test(valid_warn_log_levels) :-
  set_log_level(warn,info),
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[warn,error,fatal]]),
  set_log_level(info,warn).

test(valid_fatal_log_levels) :-
  set_log_level(fatal,info),
  findall(ValidLevels,log4p:valid_log_levels(ValidLevels),[[fatal]]),
  set_log_level(info,fatal).

:- end_tests(log4p).
