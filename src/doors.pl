:- module(doors, [door/5, room/5]).
:- use_module(security).
:- use_module(attack).

/*
            X - X i X
            |   |   |
            X   S   G
            |       |
            I - X g X








*/

door(0, 0, 0, 1, Lock):- lock_is_open(Lock).
door(0, 1, 1, 1, Lock):- lock_is_iron(Lock).
door(0, 1, -1, 1, Lock):- lock_is_open(Lock).
door(-1, 1, -1, 0, Lock):- lock_is_open(Lock).
door(-1, 0, -1, -1, Lock):- lock_is_open(Lock).
door(-1, -1, 0, -1, Lock):- lock_is_open(Lock).
door(0, -1, 1, -1, Lock):- lock_is_gold(Lock).
door(1, -1, 1, 0, Lock):- lock_is_open(Lock).
door(1, 0, 1, 1, Lock):- lock_is_open(Lock).

% room(X, Y, Items, Description)

room(0, 0, [], Empty, "You find yourself in a cold, empty, humid room. The only light comes from a torch in your hand."):- no_enemy(Empty).
room(-1, 1, [], Empty, "Upper left"):- no_enemy(Empty).
room(-1, 0, [], Empty, "Center left"):- no_enemy(Empty).
room(-1, -1, [], Empty, "Bottom left"):- no_enemy(Empty).
room(0, -1, [Key], Empty, "Bottom center"):- key_is_iron(Key), no_enemy(Empty).
room(1, -1, [], Empty, "Bottom right"):- no_enemy(Empty).
room(0, 1, [Key], Empty, "Center right"):- key_is_gold(Key), no_enemy(Empty).
room(1, 1, [], Empty, "Top right"):- no_enemy(Empty).

% for some reason, this makes prolog loop forever checking alternation door directions
% door(X, Y, X1, Y1, Lock):- door(X1, Y1, X, Y, Lock), !; door(X, Y, X1, Y1, Lock).