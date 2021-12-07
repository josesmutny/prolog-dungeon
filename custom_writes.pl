/*
 * Contains write functions used throughout the application
 */
:- module(custom_writes, [write_inventory/0, write_position/2, write_status/0]).
:- use_module(common).
:- use_module(doors).
:- use_module(write_digits).
:- use_module(security).

has_n_keys_of_type(N, Type):-
	has_keys(Keys),
        Key is Type,
	(
		member(Key, Keys), has_n_of(Key, Keys, N), !;
		not(member(Key, Keys)), N is 0
	).


are_equal([A,_], [B,_]):-
	A == B.

has_only_non_zero_values(List, NonZeroList):-
	exclude(are_equal([0,_]), List, NonZeroList).


write_items(Items):-
	has_only_non_zero_values(Items, NonZeroItems),
	write_non_zero_values(NonZeroItems).

write_with_s(Count, String):-
	(
		Count is 1, write(String), !;
		not(Count is 1), write(String), write("s")
	).

write_non_zero_values([]):- write("no items").

write_non_zero_values([[Count1, Description1], [Count2, Description2]]):-
	write(Count1), write(" "),  write_with_s(Count1, Description1), write(" and "), write(Count2), write(" "), write_with_s(Count2, Description2), !.

write_non_zero_values([[Count, Description]|[]]):-
	write(Count), write(" "), write_with_s(Count, Description), !.

write_non_zero_values([[Count, Description]| Tail]):-
	write(Count), write(" "), write_with_s(Count, Description), write(', '),
	write_non_zero_values(Tail).



write_position(X, Y):-
	write("("), write(X), write(","), write(Y), write(")").

write_inventory():-
	write("You have "),
	key_is_gold(Gold),
	has_n_keys_of_type(GoldKeyCount, Gold),
	key_is_iron(Iron),
	has_n_keys_of_type(IronKeyCount, Iron),
	has_teleport(X, Y),
	format(string(TeleportLocation), "teleport to (~d,~d)", [X, Y]),
	write_items([[IronKeyCount, "iron key"], [GoldKeyCount, "golden key"], [1, TeleportLocation]]),
	write(".").

write_gold_keys():-
	has_keys(Keys), member(Key, Keys), key_is_gold(Key), has_n_of(Key, Keys, KeyCount),
	(
		not(key_is_gold(Key) is 0), !;

		write(KeyCount), write(" golden"), (
			KeyCount is 1, write(" key"), !;
			not(KeyCount is 1), write(" keys"), !
		), !
	).

write_iron_keys():-
	has_keys(Keys), member(Key, Keys), key_is_iron(Key), has_n_of(Key, Keys, KeyCount),
	write(KeyCount), write(" iron"), (
		KeyCount is 1, write(" key"), !;
		not(KeyCount is 1), write(" keys"), !
	).

write_status():-
    position(X, Y),
    format(string(RoomDescription), "You are in the room at [~d, ~d]. ", [X, Y]),
    write(RoomDescription),
    Xup is X + 1, Xdown is X - 1, Yup is Y + 1, Ydown is Y - 1,
    (
       (door(X, Y, Xup, Y, _); door(Xup, Y, X, Y, _)), write("There is door to the south"), !;
       (door(X, Y, X, Yup, _); door(X, Yup, X, Y, _)), write("There is a door to the north"), !
    ).