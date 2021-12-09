/*
 * Contains write functions used throughout the application
 */
:- module(custom_writes, [
    write_inventory/0,
    write_position/2,
    write_status/0,
    write_delayed/3,
    write_delayed/2,
    write_delayed/1
]).

:- use_module(common).
:- use_module(doors).
:- use_module(number_to_english).
:- use_module(security).
:- use_module(navigation).

is_no_delay(0).
is_very_short_delay(0.01).
is_short_delay(0.01).
is_normal_delay(0.5).
is_long_delay(0.85).
is_very_long_delay(1.2).

:- discontiguous write_delayed/1.
write_delayed_list(['\n'], Delay, FinalDelay):- sleep(Delay), sleep(FinalDelay), write('\n'), !.
write_delayed_list([Elem], Delay, FinalDelay):- sleep(Delay), write(Elem), flush_output, sleep(FinalDelay), !.
write_delayed_list([Head|Tail], Delay, FinalDelay):-
    sleep(Delay),
    write(Head),
    flush_output,
    write_delayed_list(Tail, Delay, FinalDelay).

write_delayed(String, Delay, DelayLast):-
    atom_chars(String, List),
    write_delayed_list(List, Delay, DelayLast).

write_delayed(String):-
    is_short_delay(Delay),
    is_no_delay(FinalDelay),
    write_delayed(String, Delay, FinalDelay).

write_delayed(String, DelayLast):-
    is_short_delay(Delay),
    write_delayed(String, Delay, DelayLast).

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
		Count is 1, write_delayed(String), !;
		not(Count is 1), write_delayed(String), write_delayed("s")
	).

write_non_zero_values([]):- write_delayed("no items").

write_non_zero_values([[Count1, Description1], [Count2, Description2]]):-
    number_to_english(Count1, ECount1, Description1),
    number_to_english(Count2, ECount2, Description2),
	write_delayed(ECount1),
	write_delayed(" "),
	write_with_s(Count1, Description1),
	write_delayed(" and "),
	write_delayed(ECount2),
	write_delayed(" "),
	write_with_s(Count2, Description2), !.

write_non_zero_values([]):- !.
write_non_zero_values([[Count, Description]| Tail]):-
    number_to_english(Count, ECount, Description),
	write_delayed(ECount),
	write_delayed(" "),
	write_with_s(Count, Description),
	write_delayed(', '),
	write_non_zero_values(Tail).


write_position(X, Y):-
	write_delayed("("), write_delayed(X), write_delayed(","), write_delayed(Y), write_delayed(")").

write_inventory():-
	write_delayed("You have "),
	key_is_gold(Gold),
	has_n_keys_of_type(GoldKeyCount, Gold),
	key_is_iron(Iron),
	has_n_keys_of_type(IronKeyCount, Iron),
	(
	    has_teleport(X, Y),
	    format(string(TeleportLocation), "teleport to (~d,~d)", [X, Y]),
	    TeleportCount is 1, !;
	    not(has_teleport(_, _)), TeleportCount is 0, !
    ),
    write_items([[IronKeyCount, "iron key"], [GoldKeyCount, "golden key"], [TeleportCount, TeleportLocation]]),
    is_long_delay(Delay),
    write_delayed(".\n", Delay).

write_gold_keys():-
	has_keys(Keys), member(Key, Keys), key_is_gold(Key), has_n_of(Key, Keys, KeyCount),
	(
		not(key_is_gold(Key) is 0), !;

		write_delayed(KeyCount, 0), write_delayed(" golden"), (
			KeyCount is 1, write_delayed(" key"), !;
			not(KeyCount is 1), write_delayed(" keys"), !
		), !
	).

write_iron_keys():-
	has_keys(Keys), member(Key, Keys), key_is_iron(Key), has_n_of(Key, Keys, KeyCount),
	number_to_english(KeyCount, EnglishKeyCount, "iron"),
	write_delayed(EnglishKeyCount), write_delayed(" iron"), (
		KeyCount is 1, write_delayed(" key"), !;
		not(KeyCount is 1), write_delayed(" keys"), !
	).

write_status():-
    position(X, Y),
    room(X, Y, _, _, RoomDescription),
    /* format(string(Location), "You are in the room at [~d, ~d].\n", [X, Y]),
    concat(Location, RoomDescription, CompleteDescription), */
    is_long_delay(Delay),
    write_delayed(RoomDescription, Delay), nl,
    write_inventory(),
    write_delayed("\n"),
    (
        north_door(X, Y), write_delayed("There is a door to the north.\n", Delay), false;
        south_door(X, Y), write_delayed("There is a door to the south.\n", Delay), false;
        east_door(X, Y), write_delayed("There is a door to the east.\n", Delay), false;
        west_door(X, Y), write_delayed("There is a door to the west.\n", Delay), false;
        true
    ).
