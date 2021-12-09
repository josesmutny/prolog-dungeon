:- begin_tests(number_to_english).
:- use_module("../src/number_to_english").

test("Digits without string"):-
    number_to_english(1, String),
    String = "one",
    number_to_english(11, String2),
    String2 = "eleven",
    number_to_english(19, String3),
    String3 = "nineteen".

test("Tens without string"):-
    number_to_english(20, String1),
    String1 = "twenty",
    number_to_english(99, String2),
    String2 = "ninety nine",
    number_to_english(71, String3),
    String3 = "seventy one".

test("One with string"):-
    number_to_english(1, String1, "a"),
    String1 = "an",
    number_to_english(1, String2, "b"),
    String2 = "one".

test("Digits with string"):-
     number_to_english(20, String1, "a"),
    String1 = "twenty",
    number_to_english(99, String2, "a"),
    String2 = "ninety nine",
    number_to_english(71, String3, "a"),
    String3 = "seventy one",
    number_to_english(20, String4, "b"),
    String4 = "twenty",
    number_to_english(99, String5, "b"),
    String5 = "ninety nine",
    number_to_english(71, String6, "b"),
    String6 = "seventy one",
    number_to_english(11, String8, "a"),
    String8 = "eleven",
    number_to_english(19, String9, "a"),
    String9 = "nineteen",
    number_to_english(11, String11, "b"),
    String11 = "eleven",
    number_to_english(19, String12, "b"),
    String12 = "nineteen".

:- end_tests(number_to_english).
