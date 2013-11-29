:- use_module(library(maybe)).

:- use_module(library(tap)).


not(is_just(nothing)).
is_just(just(a)).
is_just(just(3)).
is_just(just(9.7)).
is_just(just(_)).
is_just(just([hi])).

is_nothing(nothing).
not(is_nothing(just(a))).
not(is_nothing(just(_))).

just_value(just("alpha"), "alpha").
just_value(just([]), []).
'exceptional just_value/2'(throws(_)) :-
    just_value(nothing, _).

not(maybe_value(nothing, _)).
maybe_value(just(hi), hi).
maybe_value(just([7]), [7]).

maybe_list(nothing, []).
maybe_list(just(a), [a]).
maybe_list(just(a), [a,b,c]).

maybe_default_value(nothing, 42, 42).
maybe_default_value(just(14), 42, 14).

map_maybe(succ, nothing, nothing).
map_maybe(succ, just(2), just(3)).
map_maybe(atom_string, just(hello), just("hello")).
