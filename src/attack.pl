:- module(attack, [
    is_sword/1,
    is_axe/1,
    is_ogre/1,
    is_ghost/1,
    is_orc/1,
    remove_life/0,
    no_enemy/1,
    is_enemy_description/2,
    is_weapon_description/2,
    is_death_message/2,
    can_kill/2,
    is_kill_message/3,
    is_defeat_message/2,
    attack/0
]).
:- use_module(common).

is_sword(Item):- Item is -1.
is_axe(Item):- Item is -2.

is_weapon_description(Item, "sword"):- is_sword(Item).
is_weapon_description(Item, "axe"):- is_axe(Item).

no_enemy(Entity):- Entity is 0.
is_ogre(Entity):- Entity is 1, !; Entity is -1, !.
is_ghost(Entity):- Entity is 2, !; Entity is -2, !.
is_orc(Entity):- Entity is 3, !; Entity is -3, !.

enemy_is_dead(Enemy):- Enemy < 0.

is_ogre_description("ogre").
is_ghost_description("ghost").
is_orc_description("orc").

is_enemy_description(Entity, Message):-
    (
        is_ogre(Entity), is_ogre_description(Message), !;
        is_orc(Entity), is_orc_description(Message), !;
        is_ghost(Entity), is_ghost_description(Message), !
    ).

is_defeat_message(Entity, Message):-
    (
        is_ogre(Entity), Message = "He defeats you and smashes your head with his hands.", !;
        is_orc(Entity), Message = "The orc cuts your stomach open with his sword.", !;
        is_ghost(Entity), Message = "It screeches and sucks your soul away.", !
    ).

is_kill_message(Entity, Weapon, Message):-
    is_weapon_description(Weapon, WeaponDescription),
    (
        is_ogre(Entity), format(string(Message), "You defeat him with your ~w.", WeaponDescription), !;
        is_orc(Entity), format(string(Message), "You chop his head of with your ~w.", WeaponDescription), !;
        is_ghost(Entity), format(string(Message), "You fend it away with your ~w.", WeaponDescription), !
    ).

is_death_message(Entity, Message):-
    is_ogre(Entity), Message = "You find a dead ogre with deep wounds.", !;
    is_orc(Entity), Message = "You find a dead orc, his head missing.", !;
    is_ghost(Entity), Message = "YIC", !.

/* The cuts are necessary so that weapon_kills(_, Orc) isn't evaluated twice */
weapon_kills(Weapon, Entity):- is_sword(Weapon), is_orc(Entity), !.
weapon_kills(Weapon, Entity):- is_axe(Weapon), not(is_ghost(Entity)), !.

can_kill(Entity, Weapon):-
    has_weapons(Weapons),
    weapon_kills(Weapon, Entity),
    member(Weapon, Weapons), !.

has_one_less_life(Life1, Life2):- Life2 is Life1 - 1.

remove_life():-
    has_n_lives(LifeCount),
    has_one_less_life(LifeCount, NextLifeCount),
    retract(has_n_lives(LifeCount)),
    assert(has_n_lives(NextLifeCount)).

attack():-
    position(X, Y),
    room(X, Y, _, Enemy, _),
    (
        enemy_is_dead(Enemy), write_defeated(Enemy), !;
        no_enemy(Enemy), !;
        (
            can_kill(Enemy, Weapon), write_kill(Enemy, Weapon), DeadEnemy is 0 - Enemy, set_room_enemy(X, Y, DeadEnemy), !;
            not(can_kill(Enemy, _)), remove_life, write_defeat(Enemy), !
        ), write_lives, !
    ).
