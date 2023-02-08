:- module(test_connectors, [
  test_stdio_language_connector/1
  ]).

test_stdio_language_connector(stdio(Program, Args)) :-
  Program = path(swipl),
  Args = [
    '-s',
    'test/servers',
    '-g',
    'run_test_stdio_server'
  ].
