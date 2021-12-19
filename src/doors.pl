:- module(doors, [door/5, room/5, has_neighbours/3]).
:- use_module(security).
:- use_module(attack).
:- use_module(teleport).
:- use_module(navigation).

% door(X, Y, X1, Y1, Lock)
:- dynamic door/5.

% room(X, Y, Items, Enemies, Description)
:- dynamic room/5.


has_neighbours(X, Y, [NNeighbour, SNeighbour, ENeighbour, WNeighbour]):-
    room(X, Y, _, _, _),
    is_north(X, Y, XN, YN),
    is_south(X, Y, XS, YS),
    is_east(X, Y, XE, YE),
    is_west(X, Y, XW, YW),
    (room(XN, YN, _, _, _), NNeighbour = [XN, YN], !; NNeighbour = [], !),
    (room(XS, YS, _, _, _), SNeighbour = [XS, YS], !; SNeighbour = [], !),
    (room(XE, YE, _, _, _), ENeighbour = [XE, YE], !; ENeighbour = [], !),
    (room(XW, YW, _, _, _), WNeighbour = [XW, YW], !; WNeighbour = [], !).


% for some reason, this makes prolog loop forever checking alternation door directions
% door(X, Y, X1, Y1, Lock):- door(X1, Y1, X, Y, Lock), !; door(X, Y, X1, Y1, Lock).