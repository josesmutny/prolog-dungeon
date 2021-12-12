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
    retract(position(X, Y)),
    assert(position(X1, Y1)).

can_move(Xnext, Ynext):-
    (
        not(is_dead()), !;
        is_dead(), false, !
    ),
    position(X, Y),
    (door(X, Y, Xnext, Ynext, Lock); door(Xnext, Ynext, X, Y, Lock)),
    (
        lock_is_open(Lock);
        opens(Key, Lock), remove_key(Key), (
            unlock_door(X, Y, Xnext, Ynext, Lock);
            unlock_door(Xnext, Ynext, X, Y, Lock)
        )
    ).

is_north(X, Y, X1, Y1):- X1 is X    , Y1 is Y + 1.
is_south(X, Y, X1, Y1):- X1 is X    , Y1 is Y - 1.
is_east(X, Y, X1, Y1):-  X1 is X + 1, Y1 is Y.
is_west(X, Y, X1, Y1):-  X1 is X - 1, Y1 is Y.

north_door(X, Y, Key):- is_north(X, Y, X1, Y1), (door(X, Y, X1, Y1, Key), !; door(X1, Y1, X, Y, Key), !).
south_door(X, Y, Key):- is_south(X, Y, X1, Y1), (door(X, Y, X1, Y1, Key), !; door(X1, Y1, X, Y, Key), !).
east_door(X, Y, Key):- is_east(X, Y, X1, Y1), (door(X, Y, X1, Y1, Key), !; door(X1, Y1, X, Y, Key), !).
west_door(X, Y, Key):- is_west(X, Y, X1, Y1), (door(X, Y, X1, Y1, Key), !; door(X1, Y1, X, Y, Key), !).

move_player(X, Y, X1, Y1):-
    can_move(X1, Y1), move(X, Y, X1, Y1), write_room(), !;
    not(door(X, Y, X1, Y1, _)), write_no_door(), false;
    door(X, Y, X1, Y1, Lock), opens(Key, Lock), has_keys(Keys), not(member(Key, Keys)), write_invalid_key(), !.

north():- position(X, Y), is_north(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
south():- position(X, Y), is_south(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
east():-  position(X, Y), is_east(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
west():-  position(X, Y), is_west(X, Y, X1, Y1), move_player(X, Y, X1, Y1).
