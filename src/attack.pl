:- module(attack, [
    is_sword/1,
    is_axe/1,
    is_ogre/1,
    is_ghost/1,
    is_orc/1,
    remove_life/0,
    no_enemy/1
]).
:- use_module(common).

is_sword(Item):- Item is 1.
is_axe(Item):- Item is 2.

no_enemy(Entity):- Entity is 0.
is_ogre(Entity):- Entity is 1.
is_ghost(Entity):- Entity is 2.
is_orc(Entity):- Entity is 3.

/* The cuts are necessary so that weapon_kills(_, Orc) isn't evaluated twice */
weapon_kills(Weapon, Entity):- is_sword(Weapon), is_orc(Entity), !.
weapon_kills(Weapon, Entity):- is_axe(Weapon), not(is_ghost(Entity)), !.

has_one_less_life(Life1, Life2):- Life2 is Life1 - 1.

remove_life():-
    has_n_lives(LifeCount),
    has_one_less_life(LifeCount, NextLifeCount),
    retract(has_n_lives(LifeCount)),
    assert(has_n_lives(NextLifeCount)).