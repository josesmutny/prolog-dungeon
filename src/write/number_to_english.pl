:- module(number_to_english, [number_to_english/2, number_to_english/3]).
:- use_module(custom_writes).

:- discontiguous number_to_english/2.


number_to_english(1, English, String):-
    atom_chars(String, [Head|_]),
    (
        member(Head, ['a', 'e', 'i', 'o', 'u']), English = "an", !;
        not(member(Head, ['a', 'e', 'i', 'o', 'u'])), number_to_english(1, English), !
    ).

number_to_english(N, English, _):-
    not(N is 1),
    number_to_english(N, English).

number_to_english(1, "one"):- !.
number_to_english(2, "two"):- !.
number_to_english(3, "three"):- !.
number_to_english(4, "four"):- !.
number_to_english(5, "five"):- !.
number_to_english(6, "six"):- !.
number_to_english(7, "seven"):- !.
number_to_english(8, "eight"):- !.
number_to_english(9, "nine"):- !.
number_to_english(10, "ten"):- !.

number_to_english(11, "eleven"):- !.
number_to_english(12, "twelve"):- !.
number_to_english(13, "thirteen"):- !.
number_to_english(14, "fourteen"):- !.
number_to_english(15, "fifteen"):- !.
number_to_english(16, "sixteen"):- !.
number_to_english(17, "seventeen"):- !.
number_to_english(18, "eighteen"):- !.
number_to_english(19, "nineteen"):- !.

tens_to_english(N, Tens, Name, English):-
	N > 10,
	Tens is N // 10, 
	D is N mod 10,
	(
	    D is 0, English = Name, !;
	    not(D is 0),
	    number_to_english(D, EnglishOnes),
	    format(string(English), '~w ~w', [Name, EnglishOnes]), !
    ).

number_to_english(N, English):- tens_to_english(N, 2, "twenty", English), !.
number_to_english(N, English):- tens_to_english(N, 3, "thirty", English), !.
number_to_english(N, English):- tens_to_english(N, 4, "fourty", English), !.
number_to_english(N, English):- tens_to_english(N, 5, "fifety", English), !.
number_to_english(N, English):- tens_to_english(N, 6, "sixty", English), !.
number_to_english(N, English):- tens_to_english(N, 7, "seventy", English), !.
number_to_english(N, English):- tens_to_english(N, 8, "eighty", English), !.
number_to_english(N, English):- tens_to_english(N, 9, "ninety", English), !.
