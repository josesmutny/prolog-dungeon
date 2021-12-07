:- module(doors, [door/5]).

door(0, 0, 0, 1, Lock):- lock_is_iron(Lock).
door(0, 1, 0, 2, Lock):- lock_is_gold(Lock).

% for some reason, this makes prolog loop forever checking alternation door directions
% door(X, Y, X1, Y1, Lock):- door(X1, Y1, X, Y, Lock), !; door(X, Y, X1, Y1, Lock).