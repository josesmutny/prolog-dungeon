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
    exit/0,
    restart/0
]).
:- use_module(doors).
:- use_module(security).
:- use_module(attack).

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

clear_map():-
    retractall(door(_, _, _, _, _)),
    retractall(room(_, _, _, _, _)).

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

set_rooms([]):- !.
set_rooms([[X, Y, Items, Enemies, Description]|Tail]):-
    assert(room(X, Y, Items, Enemies, Description)),
    set_rooms(Tail).

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

set_variables(PlayerX, PlayerY, Keys, Weapons, HasTele, TeleX, TeleY, LifeCount, Rooms, LockedDoors, Walls):-
    assert(position(PlayerX, PlayerY)),
    assert(has_keys(Keys)),
    assert(has_weapons(Weapons)),
    (HasTele, assert(has_teleport(TeleX, TeleY)), !; not(HasTele)),
    assert(has_n_lives(LifeCount)),
    set_rooms(Rooms),
    set_all_doors(Rooms),
    lock_doors(LockedDoors),
    remove_doors(Walls).



set_all_doors([]).
set_all_doors([[X, Y, _, _, _]|Tail]):-
    has_neighbours(X, Y, Neighbours),
    set_doors(X, Y, Neighbours),
    set_all_doors(Tail).

set_doors(_, _, []):- !.
set_doors(X, Y, [Neighbour|Tail]):-
    (
       ([X1, Y1] = Neighbour,
       not(door(X1, Y1, X, Y, _)),
       not(door(X, Y, X1, Y1, _)),
       lock_is_open(Lock),
       assert(door(X, Y, X1, Y1, Lock)), !; true, !);

       Neighbour = [], !
    ), set_doors(X, Y, Tail).

remove_door(X, Y, X1, Y1):-
    retractall(door(X, Y, X1, Y1, _)),
    retractall(door(X1, Y1, X, Y, _)).

lock_doors([]):- !.
lock_doors([[X, Y, DirectionTerm, LockTerm]|Tail]):-
    call(LockTerm, Lock),
    call(DirectionTerm, X, Y, X1, Y1),
    remove_door(X, Y, X1, Y1),
    assert(door(X, Y, X1, Y1, Lock)),
    lock_doors(Tail).


remove_doors([]):- !.
remove_doors([[X, Y, DirectionTerm]|Tail]):-
    call(DirectionTerm, X, Y, X1, Y1),
    remove_door(X, Y, X1, Y1),
    remove_doors(Tail).

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
    clear_map,
    is_orc(Orc), is_ghost(Ghost), is_ogre(Ogre), no_enemy(Empty),
    is_sword(Sword), is_axe(Axe),
    key_is_gold(GoldKey), key_is_iron(IronKey),
    set_variables(0, 0, [], [], false, 0, 0, 3,[
            [-1,1, [[0, 0]],         Ogre,   "North west"],
            [ 0,1, [GoldKey, Sword], Empty,  "North"     ],
            [ 1,1, [Axe],            Empty,  "North east"],

            [-1, 0, [], Empty, "West"   ],
            [ 0, 0, [], Empty, "Center" ],
            [ 1, 0, [], Ghost, "East"   ],

            [-1, -1, [], Empty, "South west"],
            [ 0, -1, [IronKey], Empty, "South"     ],
            [ 1, -1, [], Orc, "South east"]
        ],
        [
            [1, 1, is_west, lock_is_iron],
            [0, -1, is_east, lock_is_gold]
        ],[
            [0, 0, is_east],
            [0, 0, is_west],
            [0, 0, is_south]
        ]
    ).
