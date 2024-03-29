:- module(write_delayed, [
    write_delayed/1,
    write_delayed/2,
    write_delayed/3,
    is_short_delay/1,
    is_long_delay/1,
    set_instant/0,
    set_delayed/0
]).

:- dynamic is_short_delay/1.
:- dynamic is_long_delay/1.

set_instant():-
    retractall(is_short_delay(_)),
    retractall(is_long_delay(_)),
    assert(is_short_delay(0)),
    assert(is_long_delay(0)).

set_delayed():-
    retractall(is_short_delay(_)),
    retractall(is_long_delay(_)),
    assert(is_short_delay(0.01)),
    assert(is_long_delay(0.33)).

:- discontiguous write_delayed/1.
write_delayed_list(['\n'], Delay, FinalDelay):- sleep(Delay), sleep(FinalDelay), write('\n'), !.
write_delayed_list([Elem], Delay, FinalDelay):- sleep(Delay), write(Elem), flush_output, sleep(FinalDelay), !.
write_delayed_list([Head|Tail], Delay, FinalDelay):-
    sleep(Delay),
    write(Head),
    flush_output,
    (
        member(Head, ['.']), sleep(FinalDelay), !;
        member(Head, [',', ';']), Tmp is FinalDelay / 2, sleep(Tmp), !;
        not(member(Head, ['.', ',', ';']))),
    write_delayed_list(Tail, Delay, FinalDelay).

write_delayed(String, Delay, DelayLast):-
    atom_chars(String, List),
    write_delayed_list(List, Delay, DelayLast).

write_delayed(String):-
    is_short_delay(Delay),
    FinalDelay = 0,
    write_delayed(String, Delay, FinalDelay).

write_delayed(String, DelayLast):-
    is_short_delay(Delay),
    write_delayed(String, Delay, DelayLast).
