:- module(doors, [door/5, room/5]).
:- use_module(security).
:- use_module(attack).
:- use_module(teleport).
/*
            X - X i X
            |   |   |
            X   S   G
            |       |
            I - X g X
*/

:- dynamic door/5.

door(0, 0, 0, 1, Lock):- lock_is_open(Lock).
door(0, 1, 1, 1, Lock):- lock_is_iron(Lock).
door(0, 1, -1, 1, Lock):- lock_is_open(Lock).
door(-1, 1, -1, 0, Lock):- lock_is_open(Lock).
door(-1, 0, -1, -1, Lock):- lock_is_open(Lock).
door(-1, -1, 0, -1, Lock):- lock_is_open(Lock).
door(0, -1, 1, -1, Lock):- lock_is_gold(Lock).
door(1, -1, 1, 0, Lock):- lock_is_open(Lock).
door(1, 0, 1, 1, Lock):- lock_is_open(Lock).

:- dynamic room/5.
% room(X, Y, Items, Enemies, Description)
room(-1, 1, [Teleport], Empty, "North west"):- no_enemy(Empty), is_teleport(Teleport, 0, 0).
room(0, 1, [Key, Weapon], Empty, "North"):- key_is_gold(Key), no_enemy(Empty), is_axe(Weapon).
room(1, 1, [], Empty, "North east"):- no_enemy(Empty).

room(-1, 0, [], Empty, "West"):- no_enemy(Empty).
room(0, 0, [], Empty, "Center"):- no_enemy(Empty).
room(1, 0, [], Empty, "East"):- no_enemy(Empty).

room(-1, -1, [], Empty, "South west"):- no_enemy(Empty).
room(0, -1, [Key], Empty, "South"):- key_is_iron(Key), no_enemy(Empty).
room(1, -1, [], Empty, "South east"):- no_enemy(Empty).

% for some reason, this makes prolog loop forever checking alternation door directions
% door(X, Y, X1, Y1, Lock):- door(X1, Y1, X, Y, Lock), !; door(X, Y, X1, Y1, Lock).