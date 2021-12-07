/*
 * Print numbers up to 99 (positive only).
 */
:- module(write_digits, [write_digits/1]).
:- discontiguous write_digits/1.

write_digits(1):- write("one"), !.
write_digits(2):- write("two"), !.
write_digits(3):- write("three"), !.
write_digits(4):- write("four"), !.
write_digits(5):- write("five"), !.
write_digits(6):- write("six"), !.
write_digits(7):- write("seven"), !.
write_digits(8):- write("eight"), !.
write_digits(9):- write("nine"), !.
write_digits(10):- write("ten"), !.

write_digits(11):- write("eleven"), !. 
write_digits(12):- write("twelve"), !.
write_digits(13):- write("thirteen"), !.
write_digits(14):- write("fourteen"), !.
write_digits(15):- write("fifteen"), !.
write_digits(16):- write("sixteen"), !.
write_digits(17):- write("seventeen"), !.
write_digits(18):- write("eighteen"), !.
write_digits(19):- write("nineteen"), !.

write_tens(N, Tens, Name):-
	N > 10,
	Tens is N // 10, 
	D is N mod 10, 
	write(Name), write(" "),
	write_digits(D).

write_digits(N):- write_tens(N, 2, "twenty"), !.
write_digits(N):- write_tens(N, 3, "thirty"), !.
write_digits(N):- write_tens(N, 4, "fourty"), !.
write_digits(N):- write_tens(N, 5, "fifety"), !.
write_digits(N):- write_tens(N, 6, "sixty"), !.
write_digits(N):- write_tens(N, 7, "seventy"), !.
write_digits(N):- write_tens(N, 8, "eighty"), !.
write_digits(N):- write_tens(N, 9, "ninety"), !.
