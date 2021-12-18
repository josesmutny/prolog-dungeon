:- use_module(src/write/custom_writes).
:- use_module(src/write/write_delayed).
:- use_module(src/doors).
:- use_module(src/navigation).
:- use_module(src/write/helpers).
:- use_module(src/security).
:- use_module(src/items).
:- use_module(src/teleport).

:-
    write_delayed("Hello traveller.\n", 0.1, 1), nl,
/*    write_delayed("An adventure awaits.\n", 1),
    write_delayed("You are trapped in a dungeon, from whence only few have come alive.\n"),
    write_delayed("I shall hold you no further. Use north()., south(). east(). and west(). to move, and collect the right keys to open the right dooors.\n"),
    write_delayed("Good luck!\n", 0.2, 2), nl, */
    write_status(), nl, write_room().