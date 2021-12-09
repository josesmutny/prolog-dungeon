:- begin_tests(security).
:- use_module("../src/security").

test(lock_is_open):- lock_is_open(0).

test(lock_is_iron):- lock_is_iron(1).

test(lock_is_gold):- lock_is_gold(2).

test(is_locked):-
    lock_is_iron(Lock1),
    is_locked(door(_, _, _, _, Lock1)),
    lock_is_gold(Lock2),
    is_locked(door(_, _, _, _, Lock2)),
    lock_is_open(Lock3),
    not(is_locked(door(_, _, _, _, Lock3))).


:- end_tests(security).