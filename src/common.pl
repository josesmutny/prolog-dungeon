:- module(common, [
    position/2,
    has_keys/1,
    has_teleport/2,
    has_n_of/3,
    has_n_lives/1,
    has_weapons/1,
    is_dead/0
]).
:- use_module(attack).


/* Memory simulation =====================================================================*/
:- dynamic position/2.
position(0, 0).

:- dynamic has_keys/1.
has_keys([1, 1, 1, 2]).

:- dynamic has_teleport/2.
has_teleport(0, 1).

:- dynamic has_weapons/1.
has_weapons([Sword]):- is_sword(Sword).

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

set_enemies([]).
set_enemies([[X, Y, Enemy]|Tail]):-
    assert(has_enemy(X, Y, Enemy)),
    set_enemies(Tail).

set_variables(PlayerX, PlayerY, Keys, Weapons, HasTele, TeleX, TeleY, LifeCount, Enemies):-
    assert(position(PlayerX, PlayerY)),
    assert(has_keys(Keys)),
    assert(has_weapons(Weapons)),
    (HasTele, assert(has_teleport(TeleX, TeleY)), !; not(HasTele)),
    assert(has_n_lives(LifeCount)),
    set_enemies(Enemies).

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
