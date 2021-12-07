:- module(common, [
    position/2,
    has_keys/1,
    has_teleport/2,
    has_n_of/3
]).

/* Memory simulation =====================================================================*/
:- dynamic position/2.
position(0, 0).

:- dynamic has_keys/1.
has_keys([1]).

:- dynamic has_teleport/2.
has_teleport(0, 1).

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
