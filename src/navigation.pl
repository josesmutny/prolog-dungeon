:- module(navigation, [
    north/0,
    south/0,
    east/0,
    west/0,
    north_door/3,
    south_door/3,
    east_door/3,
    west_door/3
]).
:- use_module(common).
:- use_module(write/custom_writes).
:- use_module(security).
:- use_module(doors).

% Move functions ==================================================================================

move(X, Y, X1, Y1):-
    write_position(X1, Y1), nl,
    retract(position(X, Y)),
    assert(position(X1, Y1)),
    write_status().

can_move(Xnext, Ynext):-
    (
        not(is_dead()), !;
        is_dead(), false, !
    ),
    position(X, Y),
    (door(X, Y, Xnext, Ynext, Lock); door(Xnext, Ynext, X, Y, Lock)),
    opens(Key, Lock).

is_north(X, Y, X1, Y1):- X1 is X    , Y1 is Y + 1.
is_south(X, Y, X1, Y1):- X1 is X    , Y1 is Y - 1.
is_east(X, Y, X1, Y1):-  X1 is X + 1, Y1 is Y.
is_west(X, Y, X1, Y1):-  X1 is X - 1, Y1 is Y.

north_door(X, Y):- is_north(X, Y, X1, Y1), (door(X, Y, X1, Y1, _), !; door(X1, Y1, X, Y, _), !).
south_door(X, Y):- is_south(X, Y, X1, Y1), (door(X, Y, X1, Y1, _), !; door(X1, Y1, X, Y, _), !).
east_door(X, Y):- is_east(X, Y, X1, Y1), (door(X, Y, X1, Y1, _), !; door(X1, Y1, X, Y, _), !).
west_door(X, Y):- is_west(X, Y, X1, Y1), (door(X, Y, X1, Y1, _), !; door(X1, Y1, X, Y, _), !).

move_player(X, Y, X1, Y1):-
    can_move(X1, Y1), move(X, Y, X1, Y1), !;
    not(door(X, Y, X1, Y1, _)), custom_writes:is_long_delay(Delay), write_delayed("There is no door.\n", Delay), !;
    door(X, Y, X1, Y1, Lock), opens(Key, Lock), has_keys(Keys), not(member(Key, Keys)), write("You do not have the right key."), nl, !.

north():- position(X, Y), is_north(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
south():- position(X, Y), is_south(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
east():-  position(X, Y), is_east(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
west():-  position(X, Y), is_west(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
