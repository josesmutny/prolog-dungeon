:- module(helpers, [
    has_n_keys_of_type/2,
    has_n_keys_of_type/3,
    has_unique_items/2,
    has_only_non_zero_values/2,
    has_s_if_plural/3,
    has_n_weapons_of_type/2,
    has_n_weapons_of_type/3,
    has_only_non_blank_values/2,
    is_formatted/2
]).
:- use_module("../common").


has_n_keys_of_type(N, Keys, Type):-
    Key is Type,
    (
		member(Key, Keys), has_n_of(Key, Keys, N), !;
		not(member(Key, Keys)), N is 0
    ).
has_n_keys_of_type(N, Type):- has_keys(Keys), has_n_keys_of_type(N, Keys, Type).

has_n_weapons_of_type(N, Weapons, Type):-
    Weapon is Type,
    (
        member(Weapon, Weapons), has_n_of(Weapon, Weapons, N), !;
        not(member(Weapon, Weapons)), N is 0
    ).
has_n_weapons_of_type(N, Type):- has_weapons(Weapons), has_n_weapons_of_type(N, Weapons, Type).

are_equal([A,_], [B,_]):-
	A == B.

items_are_equal(A, B):- A = B.


is_prefixed(Item, List, ExtendedList):- ExtendedList = [Item | List].

has_only_non_zero_values(List, NonZeroList):-
	exclude(are_equal([0,_]), List, NonZeroList).

has_only_non_blank_values(List, NotEmptyList):-
    exclude(items_are_equal(""), List, NotEmptyList).

is_reversed(List, ReversedList):-
    is_reversed(List, [], ReversedList).

is_reversed([], List, List):- !.
is_reversed([Item], [], [Item]):- !.
is_reversed([Head|Tail], Buffer, Reversed):-
    NewBuffer = [Head | Buffer],
    is_reversed(Tail, NewBuffer, Reversed).

has_unique_items(List, Unique):- has_unique_items(List, [], UniqueReversed), is_reversed(UniqueReversed, Unique).

has_unique_items([], List, List):- !.
has_unique_items([Item], [], [Item]):- !.
has_unique_items([Item|Tail], Buffer, Unique):-
    (
        member(Item, Tail), has_unique_items(Tail, Buffer, Unique), !;
        not(member(Item, Tail)), NewBuffer = [Item | Buffer], has_unique_items(Tail, NewBuffer, Unique), !
    ).

has_s_if_plural(String, Count, StringWithS):-
    (
        Count is 1, StringWithS = String, !;
        not(Count is 1), format(string(StringWithS), "~ws", [String])
    ).

% [a, b, c] -> 'a\nb\nc\n'
is_formatted([], _):- !.
is_formatted([Head], String):-
    format(string(String), "~w\n", [Head]), !.
is_formatted([Head, Last], String):-
    format(string(String), "~w\n~w\n", [Head, Last]), !.
is_formatted([Head|Tail], String):-
    is_formatted(Tail, FormattedTail),
    format(string(String), "~w\n~w", [Head, FormattedTail]).