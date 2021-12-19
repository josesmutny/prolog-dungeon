:- module(items, [
    is_teleport/1,
    is_key/1,
    is_weapon/1,
    is_room_item_description/2,
    pick/0
]).
:- use_module(common).
:- use_module(attack).
:- use_module(teleport).


is_key(Item):-
    key_is_iron(Item), !;
    key_is_gold(Item), !.
is_weapon(Item):-
    is_axe(Item), !;
    is_sword(Item), !.


is_room_item_description(Item, String):-
    (
        is_teleport(Item), is_teleport_description(Item, String), !;
        is_key(Item), is_key_description(Item, String), !;
        is_weapon(Item), is_weapon_description(Item, String), !
    ), !.

% == Item interaction ==============================================================================================

has_n_items(N):-
    has_keys(Keys), length(Keys, KeyCount),
    (has_teleport(_, _), TeleportCount is 1, !; TeleportCount is 0),
    has_weapons(Weapons), length(Weapons, WeaponCount),
    N is KeyCount + TeleportCount + WeaponCount.

pick():-
    (not(is_dead),
    has_n_items(ItemCount),
    position(X, Y),
    (
        ItemCount < 5,
        room(X, Y, [Item|Rest], _, _),
        (
            is_teleport(Item), set_teleport(Item), write_picked_teleport(Item), !;
            is_key(Item), has_keys(Keys), set_keys([Item|Keys]), write_picked_key(Item), !;
            is_weapon(Item), has_weapons(Weapons), set_weapons([Item|Weapons]), write_picked_weapon(Item), !
        ), set_room_contents(X, Y, Rest), !;

        room(X, Y, [], _, _), write_no_items_to_pick(), false;
        ItemCount is 5, write_full_inventory(), false
    ), !);
    write_death, false, !.
