:- module(common, [
    position/2,
    has_keys/1,
    has_teleport/2,
    has_n_of/3,
    has_n_lives/1,
    has_weapons/1,
    is_dead/0,
    remove_key/1,
    set_keys/1,
    set_weapons/1,
    set_teleport/1,
    set_room_contents/3,
    set_room_enemy/3,
    unlock_door/5,
    clear_teleport/0,
    exit/1,
    restart/1
]).
:- use_module(attack).
:- use_module(doors).

/* Memory simulation =====================================================================*/
:- dynamic position/2.
position(0, 0).

:- dynamic has_keys/1.
has_keys([]).

:- dynamic has_teleport/2.
has_teleport(0, 1).

:- dynamic has_weapons/1.
has_weapons([]).

:- dynamic has_n_lives/1.
has_n_lives(3).
is_dead():- has_n_lives(N), N < 1.

:- dynamic has_enemy/3.
has_enemy(0, 1, Enemy):- is_orc(Enemy).


clear_variables():-
    retractall(position(_, _)),
    retractall(has_keys(_)),
    retractall(has_teleport(_, _)),
    retractall(has_weapons(_)),
    retractall(has_n_lives(_)),
    retractall(has_enemy(_, _, _)).

set_keys(List):-
    retractall(has_keys(_)),
    assert(has_keys(List)).

remove_first([], _, []):- !.
remove_first([Item], Item, []):- !.
remove_first([Item|Tail], Item, Tail):- !.
remove_first([Head|Tail], Item, [Head|ListWithout]):-
    Head \= Item,
    remove_first(Tail, Item, ListWithout), !.

clear_keys():-
    retractall(has_keys(_)).

remove_key(Key):-
    has_keys(Keys),
    member(Key, Keys),
    remove_first(Keys, Key, NewKeys),
    clear_keys(),
    set_keys(NewKeys), !.


unlock_door(X, Y, Xnext, Ynext, Lock):-
    retractall(door(X, Y, Xnext, Ynext, Lock)),
    retractall(door(Xnext, Ynext, X, Y, Lock)),
    lock_is_open(Unlocked), asserta(door(X, Y, Xnext, Ynext, Unlocked)).

set_enemies([]).
set_enemies([[X, Y, Enemy]|Tail]):-
    set_room_enemy(X, Y, Enemy),
    set_enemies(Tail).

set_teleport([X, Y]):-
    retractall(has_teleport(_, _)),
    assert(has_teleport(X, Y)).

set_weapons(Weapons):-
    retractall(has_weapons(_)),
    assert(has_weapons(Weapons)).

set_room_contents(X, Y, Contents):-
    room(X, Y, _, Enemy, Description),
    NewEnemy is Enemy, NewDescription = Description,
    retractall(room(X, Y, _, _, _)),
    assert(room(X, Y, Contents, NewEnemy, NewDescription)).

set_room_enemy(X, Y, Enemy):-
    room(X, Y, Contents, _, Description),
    NewContents = Contents, NewDescription = Description,
    retractall(room(X, Y, _, _, _)),
    assert(room(X, Y, NewContents, Enemy, NewDescription)).

set_variables(PlayerX, PlayerY, Keys, Weapons, HasTele, TeleX, TeleY, LifeCount, Enemies):-
    assert(position(PlayerX, PlayerY)),
    assert(has_keys(Keys)),
    assert(has_weapons(Weapons)),
    (HasTele, assert(has_teleport(TeleX, TeleY)), !; not(HasTele)),
    assert(has_n_lives(LifeCount)),
    set_enemies(Enemies).

clear_teleport():-
    retractall(has_teleport(_, _)).

/* List utilities ========================================================================*/
has_n_of(_, [], 0).
has_n_of(Elem, List, 0):- not(member(Elem, List)).
has_n_of(Elem, [Elem], 1).
has_n_of(Elem, [Elem2], 0):- not(Elem is Elem2).
has_n_of(Elem, [Head | Tail], N):-
	not(Tail = []),
	has_n_of(Elem, Tail, N1),
	(
		Elem is Head, N is N1+ 1, !;
		not( Elem is Head), N is N1
	), !.

% =========================================================================================

exit():-
    halt.

restart():-
    clear_variables,
    set_variables(0, 0, [], [], false, 0, 0, 3,
        [[X1, Y1, Enemy1],
        [X2, Y2, Enemy2],
        [X3, Y3, Enemy3]]
    ).