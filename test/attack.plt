:- begin_tests(attack).
:- use_module("../src/attack").
:- use_module("../src/common").

test(is_sword):- is_sword(1).

test(is_axe):- is_axe(2).

test(is_ogre):- is_ogre(1).

test(is_ghost):- is_ghost(2).

test(is_orc):- is_orc(3).

test(weapon_kills):-
    is_ogre(Ogre), is_ghost(Ghost), is_orc(Orc),
    is_sword(Sword), is_axe(Axe),
    attack:weapon_kills(Sword, Orc),
    not(attack:weapon_kills(Sword, Ogre)),
    not(attack:weapon_kills(Sword, Ghost)),
    not(attack:weapon_kills(Axe, Ghost)),
    attack:weapon_kills(Axe, Ogre),
    attack:weapon_kills(Axe, Orc).

test(has_one_less_life):-
    attack:has_one_less_life(0, -1),
    attack:has_one_less_life(1, 0).

test(remove_life):-
    common:clear_variables(),
    common:set_variables(0, 0, [], [], false, 0, 0, 2, []),
    common:has_n_lives(2),
    remove_life(),
    common:has_n_lives(1),
    remove_life(),
    common:has_n_lives(0).

:- end_tests(attack).