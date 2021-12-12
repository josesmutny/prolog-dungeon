:- module(teleport, [
    is_teleport_description/2,
    is_teleport/1,
    is_teleport/3
]).

is_teleport(Teleport):- [_, _] = Teleport.
is_teleport(Teleport, X, Y):- [X, Y] = Teleport.

is_teleport_description([X, Y], String):-
    format(string(String), "teleport to (~d, ~d)", [X, Y]).