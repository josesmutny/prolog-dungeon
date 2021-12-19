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
:- use_module("write/write_delayed").

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

:- dynamic has_king/1.

clear_variables():-
    retractall(position(_, _)),
    retractall(has_keys(_)),
    retractall(has_teleport(_, _)),
    retractall(has_weapons(_)),
    retractall(has_n_lives(_)).

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
		Elem = Head, N is N1+ 1, !;
		Elem \= Head, N is N1
	), !.

% =========================================================================================

exit():-
    halt.

restart():-
    set_delayed,
    clear_variables,
    clear_map,
    is_orc(Orc), is_ghost(Ghost), is_ogre(Ogre), no_enemy(Empty),
    is_sword(Sword), is_axe(Axe),
    key_is_gold(GoldKey), key_is_iron(IronKey),
    set_variables(0, 0, [], [], false, 0, 0, 3,[
            [-1, 1, [], Empty, "This room reeks of rotten flesh. You hear whispers, but they go away as you turn around. On the wall hang the ragged remains of an old painting, depicting a luxurios banquet."],
            [-1, 0, [Sword, [-1, 1]], Orc, "This room has a different floor, made of much coarser stone. The walls are also different, and it seems you are going deeper."],
            [-1, -1, [[4, 0]], Empty, "You find yourself in a treasure room. Around you there are chests filled with gold, and a not so old painting of a king with his queen."],

            [0, 1, [IronKey], Empty, "There is a torch in the wall, but apart from a rat running away you are alone. After turning around you see a skeleton clenching something in his fist."],
            [0, 0, [], Empty, "You find yourself in an empty dungeon. The only door seems to be broken, and you see a dim light coming from the next room."],

            [1, 1, [Sword], Empty, "The walls are getting wider and the ceiling is higher. An eye spies at you, but when you shine towards it is only a cat."],

            [2, 2, [], Orc, "The light is dim in this room, and you can only make the frame of what looks like a door."],
            [2, 1, [[2, 0], IronKey, GoldKey], Empty, "Behind the door you find the guards quarters. There is a small chest on the corner, and a half torn painting of a king hangs on the wall."],
            [2, 0, [GoldKey, GoldKey], Empty, "This room is covered in cobweb. After cutting through it you find a skeleton covering a small bag. You hear thumping in the distance, but cannot say whence it came."],

            [3, 3, [Axe], Ogre, "The room is filled with a horrid stench. The floor is covered in shattered bones."],
            [3, 2, [], Orc, "You are in a great room, with larger stone pillars going high up into the dark ceiling. You hear thundering steps nearing."],
            [3, 1, [], Ghost, "The room is cold as the stone beneath your feet. Every thing is covered by dust so thick that the whole room seems dead."],
            [3, 0, [], Ogre, "You feel bones crushing beneath your feet, and pieces of torn armor lay around."],
            [3, -1, [IronKey], Empty, "What seems like the remains of a prisoner is crouching in the corner. He has something in his hands"],

            [4, 3, [[6, 1]], Empty, "You arrive at a luxourious room. The walls glisten with gold, and before you lies a small chest shining with gems. A purple gown is hanged on the wall behind you."],
            [4, 2, [], Ogre, "You nearly vomit and stumble, to find the floor is covered in bones."],
            [4, 1, [], Sword, "This room has a collection of paintings, one older than the next. Over the newest one a sword hangs"],
            [4, 0, [], Empty, "The walls are white as snow, and the only door is slightly open. You feel cold seeping in through the small gap."],
            [4, -1, [IronKey], Ghost, "The tiny room has only a small chest in the corner, a spider makes it web on the corner."],

            [5, 1, [GoldKey], Ghost, "After going down some stairs you find yourself in a crypt. One of the graves is open."],
            [5, 0, [], Ghost, "The walls seem to be right on top of your head, and you have to crouch to continue."],
            [5, -1, [[4, 3]], Ogre, "After going down some stairs you feel the stench in the air."],

            [6, 1, [], Orc, "The room is totally dark, and the walls have been stripped of any decorations."],
            [6, 0, [], Empty, "You have reached the King!. He is sleepy, but after telling him your story he seems encouraged. He asks you to take him back with you, if you can."],
            [6, -1, [], Orc, "The room is completely empty, apart from a broken helmet in the corner"]
        ],
        [
            [0, 1, is_east, lock_is_iron],
            [-1, 0, is_south, lock_is_iron],
            [2, 2, is_south, lock_is_iron],
            [3, 1, is_east, lock_is_iron],
            [3, -1, is_east, lock_is_gold],
            [4, -1, is_east, lock_is_gold],
            [5, -1, is_east, lock_is_gold],
            [5, 0, is_east, lock_is_gold]
        ],[
            [0, 0, is_west],
            [2, 1, is_south],
            [2, 1, is_east],
            [3, 2, is_south],
            [4, 2, is_south],
            [4, 0, is_south],
            [4, 0, is_west],
            [4, 0, is_north],
            [5, 0, is_south]
        ]
    ).
