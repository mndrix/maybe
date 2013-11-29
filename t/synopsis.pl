:- use_module(library(maybe)).

staff(tom, just(teresa)).
staff(bob, nothing).
staff(sue, just(william)).

:- use_module(library(tap)).

synopsis :-
    findall( Name-Spouse
           , ( staff(Name, MaybeSpouse)
             , maybe_default_value(MaybeSpouse, single, Spouse)
             )
           , Couples
           ),
    Couples == [ tom-teresa
               , bob-single
               , sue-william
               ].
