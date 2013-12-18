:- use_module(library(maybe)).
:- use_module(library(quickcheck)).

xor(A,B) :-
    ( call(A), \+ call(B)
    ; call(B), \+ call(A)
    ),
    !.


% define some properties

just_or_nothing(Maybe:maybe(atomic)) :-
    xor( is_just(Maybe)
       , is_nothing(Maybe)
       ).

maybe_value_or_nothing(Maybe:maybe(atomic)) :-
    xor( maybe_value(Maybe, _)
       , is_nothing(Maybe)
       ).

wrap_unwrap(X:atomic) :-
    just_value(Just, X),
    just_value(Just, Y),
    X == Y.

list_round_trip(L0:list(atomic)) :-
    maybe_list(Maybe, L0),
    maybe_list(Maybe, L1),
    xor( (L0 = [H|_], L1 = [H|_])  % same head ...
       , (L0 = [],    L1 = []   )  % ... or both empty
       ).


:- use_module(library(tap)).

quickcheck(just_or_nothing/1).
quickcheck(maybe_value_or_nothing/1).
quickcheck(wrap_unwrap/1).
quickcheck(list_round_trip/1).
