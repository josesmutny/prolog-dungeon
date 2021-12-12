:- module(custom_writes, [
    write_status/0,
    write_room/0,
    write_invalid_key/0,
    write_no_door/0,
    write_picked_key/1,
    write_picked_teleport/1,
    write_picked_weapon/1,
    write_no_items_to_pick/0,
    write_full_inventory/0
]).

:- use_module("../common").
:- use_module("../doors").
:- use_module(number_to_english).
:- use_module("../security").
:- use_module("../navigation").
:- use_module(write_delayed).
:- use_module(helpers).
:- use_module("../attack").
:- use_module("../items").
/* =========================================================================== */
is_room_description(String):-
    position(X, Y),
    room(X, Y, _, _, String), !.
/* === General status ======================================================== */

is_item_status(Count, Description, String):-
      number_to_english(Count, EnglishCount, Description),
      has_s_if_plural(Description, Count, DescriptionWithS),
      format(string(String), "~w ~w", [EnglishCount, DescriptionWithS]).

is_appended_status(_, [], String, String):- !.

is_appended_status(StatusTerm, [Item], Prefix, String):-
    call(StatusTerm, Item, ItemStatus),
    format(string(String), "~w~w.", [Prefix, ItemStatus]), !.

is_appended_status(StatusTerm, [Item1, Item2], Prefix, String):-
    call(StatusTerm, Item1, Status1),
    call(StatusTerm, Item2, Status2),
    format(string(String), "~w~w and ~w.", [Prefix, Status1, Status2]), !.

is_appended_status(StatusTerm, [Head|Tail], Prefix, String):-
    call(StatusTerm, Head, Status),
    format(string(NewPrefix), "~w~w, ", [Prefix, Status]),
    is_appended_status(StatusTerm, Tail, NewPrefix, String).
/* === Key status ============================================================ */
% you have an iron key and two golden keys. or ""

is_single_key_status(Key, String):-
     has_n_keys_of_type(Count, Key),
     is_key_description(Key, Description),
     is_item_status(Count, Description, String).

is_key_status(String):-
    has_keys(Keys),
    has_unique_items(Keys, UniqueKeys), % otherwhise every key writes that there are N of itself (ther ar thre, there are three, there are three...)
    length(UniqueKeys, KeyCount),
    (
        KeyCount \= 0, is_appended_status(is_single_key_status, UniqueKeys, "You have ", String), !;
        KeyCount = 0, String = "", !
    ).

% === Weapon status ============================================================

is_single_weapon_status(Weapon, String):-
    has_n_weapons_of_type(Count, Weapon),
    is_weapon_description(Weapon, Description),
    is_item_status(Count, Description, String).

is_weapon_status(String):-
    has_weapons(Weapons),
    has_unique_items(Weapons, UniqueWeapons),
    length(UniqueWeapons, WeaponCount),
    (
        WeaponCount \= 0, is_appended_status(is_single_weapon_status, UniqueWeapons, "You are armed with ", String), !;
        WeaponCount = 0, format(string(String), "You are unarmed."), !
    ).

% === Teleport status ============================================================

is_teleport_status(String):-
    (
        has_teleport(X, Y), format(string(String), "You have a teleport device pointing to (~d, ~d).", [X, Y]), !;
        not(has_teleport(_, _)), String = "", !
    ).


% === Door status ============================================================

is_door_status(DirectionTerm, DirectionString, Status):-
    position(X, Y),
    (
        call(DirectionTerm, X, Y, Lock), (
            lock_is_open(Lock), format(string(Status), "There is an open door to the ~w.", DirectionString), !;
            is_lock_description(Lock, LockDescription),
            format(string(Status), "There is a door with ~w to the ~w.", [LockDescription, DirectionString]), !
        ), !;
        Status = "", !
    ), !.


is_available_doors(String):-
    is_door_status(north_door, "north", NorthStatus),
    is_door_status(south_door, "south", SouthStatus),
    is_door_status(east_door, "east", EastStatus),
    is_door_status(west_door, "west", WestStatus),
    has_only_non_blank_values([NorthStatus, SouthStatus, EastStatus, WestStatus], StatusList),
    is_formatted(StatusList, String).

% === Life status ============================================================

is_life_status(String):-
    has_n_lives(Lives),
    number_to_english(Lives, EnglishLives),
    has_s_if_plural("life", Lives, LifeString),
    format(string(String), "You have ~w ~w left.", [EnglishLives, LifeString]).

% === Final predicates ============================================================

is_status(String):-
    is_life_status(LifeStatus),
    is_key_status(KeyStatus),
    is_weapon_status(WeaponStatus),
    is_teleport_status(TeleportStatus),
    has_only_non_blank_values([LifeStatus, KeyStatus, WeaponStatus, TeleportStatus], StatusList),
    is_formatted(StatusList, String).

% =================================================================================

has_n_items_of_type(Count, Item):-
    position(X, Y),
    room(X, Y, Items, _, _),
    (
        is_teleport(Item), (
            has_teleport(_, _), Count is 1, !;
            not(has_teleport(_, _)), Count is 0, !
        ), !;
        is_key(Item), has_n_keys_of_type(Count, Items, Item), !;
        is_weapon(Item), has_n_weapons_of_type(Count, Items, Item), !
    ).

is_room_item_status(Item, String):-
     has_n_items_of_type(Count, Item),
     is_room_item_description(Item, Description),
     is_item_status(Count, Description, String).

% room(X, Y, Items, Enemies, Description)
is_room_items_description(String):-
    position(X, Y),
    room(X, Y, Items, _, _),
    has_unique_items(Items, UniqueItems), % otherwhise every key writes that there are N of itself (ther ar thre, there are three, there are three...)
    (
        UniqueItems = [], format(string(String), "There is nothing in the room.", []), !;
        is_appended_status(is_room_item_status, UniqueItems, "On the floor you find ", String), !
    ).



write_room():-
    is_room_description(RoomDescription),
    is_available_doors(IsAvailableDoors),
    is_long_delay(FinalDelay),
    is_room_items_description(RoomItemDescription),
    has_only_non_blank_values([RoomDescription, IsAvailableDoors, RoomItemDescription], Description),
    is_formatted(Description, FormattedDescription),
    write_delayed(FormattedDescription, FinalDelay).

write_status():-
    is_status(X),
    is_long_delay(FinalDelay),
    write_delayed(X, FinalDelay).

write_invalid_key():-
    is_long_delay(FinalDelay),
    write_delayed("The door is locked and you do not have the right key.", FinalDelay).

% ================================================================================

write_no_door():-
    is_long_delay(Delay),
    write_delayed("There is no door in this direction.", Delay).

% ================================================================================
write_picked_teleport([X, Y]):-
    is_long_delay(Delay),
    format(string(String), "You pick a teleport device pointing to (~d, ~d).\n", [X, Y]),
    write_delayed(String, Delay).

write_picked_key(Key):-
    is_key_description(Key, KeyDescription),
    number_to_english(1, KeyCount, KeyDescription),
    format(string(String), "You pick ~w ~w.\n", [KeyCount, KeyDescription]),
    is_long_delay(Delay), write_delayed(String, Delay).

write_picked_weapon(Weapon):-
    is_weapon_description(Weapon, Description),
    number_to_english(1, Count, Description),
    format(string(String), "You arm yourself with ~w ~w.\n", [Count, Description]),
    is_long_delay(Delay), write_delayed(String, Delay).

write_no_items_to_pick():-
    is_long_delay(Delay),
    write_delayed("There are no items to pick.\n", Delay).

write_full_inventory():-
    is_long_delay(Delay),
    write_delayed("Your inventory is full, you cannot pick anything new.", Delay).