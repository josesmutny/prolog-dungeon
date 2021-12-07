:- module(navigation, [
    north/0,
    south/0,
    east/0,
    west/0
]).
:- use_module(common).
% Move functions ==================================================================================

move(X, Y, X1, Y1):-
    write_position(X1, Y1), nl,
    retract(position(X, Y)),
    assert(position(X1, Y1)).

can_move(Xnext, Ynext):-
    position(X, Y),
    has_keys(Keys),
    member(Key, Keys),
    (door(X, Y, Xnext, Ynext, Lock); door(Xnext, Ynext, X, Y, Lock)),
    opens(Key, Lock).

% North ===============================================================================================
can_move_north():-
	position(X, Y),
	Ynext is Y + 1,
    can_move(X, Ynext).

move_north(X, Y):-
	Ynext is Y + 1,
	write("Moving north: "),
    move(X, Y, X, Ynext).

is_north(X, Y, X1, Y1):- X1 is X    , Y1 is Y + 1.
is_south(X, Y, X1, Y1):- X1 is X    , Y1 is Y - 1.
is_east(X, Y, X1, Y1):-  X1 is X - 1, Y1 is Y.
is_west(X, Y, X1, Y1):-  X1 is X + 1, Y1 is Y.


move_player(X, Y, X1, Y1):-
    can_move(X1, Y1), move(X, Y, X1, Y1), !;
    not(door(X, Y, X1, Y1, _)), write("There is no door."), nl, !;
    door(X, Y, X1, Y1, Lock), opens(Key, Lock), has_keys(Keys), not(member(Key, Keys)), write("You do not have the right key."), nl, !.

north():- position(X, Y), is_north(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
south():- position(X, Y), is_south(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
east():-  position(X, Y), is_east(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
west():-  position(X, Y), is_west(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
