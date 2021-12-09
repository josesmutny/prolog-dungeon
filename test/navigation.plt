:- begin_tests(navigation).
:- use_module("../src/navigation").
:- use_module("../src/common").

test("move x"):-
    retractall(position(_, _)),
    assert(position(0, 0)),
    navigation:move(0, 0, 1, 0),
    position(1, 0).

test("move y"):-
    retractall(position(_, _)),
    assert(position(0, 0)),
    navigation:move(0, 0, 0, 1),
    position(0, 1).

test("move y and y"):-
    retractall(position(_, _)),
    assert(position(0, 0)),
    navigation:move(0, 0, 1, 2),
    position(1, 2).


:- end_tests(navigation).