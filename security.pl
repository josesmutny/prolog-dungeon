:- module(security, [
    lock_is_gold/1,
    lock_is_iron/1,
    is_locked/1,
    key_is_gold/1,
    key_is_iron/1,
    opens/2
]).
:- use_module(common).

lock_is_iron(1).
lock_is_gold(2).

is_locked(door(_, _, _, _, Lock)):- not( Lock is  0).

key_is_iron(1).
key_is_gold(2).

opens(Key, Lock):- key_is_iron(Key), lock_is_iron(Lock), !; key_is_gold(Key), lock_is_gold(Lock).
