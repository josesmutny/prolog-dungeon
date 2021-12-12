:- module(security, [
    lock_is_gold/1,
    lock_is_iron/1,
    lock_is_open/1,
    door_is_locked/1,
    door_is_open/1,
    key_is_gold/1,
    key_is_iron/1,
    opens/2,
    is_key_description/2,
    is_lock_description/2
]).
:- use_module(common).

lock_is_iron(1).
lock_is_gold(2).
lock_is_open(3).

is_lock_description(Lock, "an iron lock"):- lock_is_iron(Lock).
is_lock_description(Lock, "a golden lock"):- lock_is_gold(Lock).

door_is_locked(door(_, _, _, _, Lock)):- not(lock_is_open(Lock)).
door_is_open(door(_, _, _, _, Lock)):- lock_is_open(Lock).

key_is_iron(1).
key_is_gold(2).

is_key_description(Type, Description):-
    (
        key_is_gold(Type), Description = "gold key", !;
        key_is_iron(Type), Description = "iron key", !
    ).

opens(_, Lock):- lock_is_open(Lock).
opens(Key, Lock):- key_is_iron(Key), lock_is_iron(Lock), !; key_is_gold(Key), lock_is_gold(Lock).
