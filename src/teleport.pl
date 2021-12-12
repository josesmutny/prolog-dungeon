:- module(teleport, [
    is_teleport_description/2,
    is_teleport/1,
    is_teleport/3,
    teleport/0
]).
:- use_module(common).
:- use_module(navigation).

is_teleport(Teleport):- [_, _] = Teleport.
is_teleport(Teleport, X, Y):- [X, Y] = Teleport.

is_teleport_description([X, Y], String):-
    format(string(String), "teleport to (~d, ~d)", [X, Y]).

teleport():-
    (
        has_teleport(X, Y), position(CurrX, CurrY), move(CurrX, CurrY, X, Y), clear_teleport(), write_teleported(X, Y), !;
        not(has_teleport(_, _)), write_no_teleport, !
    ).