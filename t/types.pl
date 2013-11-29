:- use_module(library(maybe)).
:- use_module(library(error), [is_of_type/2]).

:- use_module(library(tap)).


% without inner type description
is_of_type(maybe, nothing).
is_of_type(maybe, just(_)).
is_of_type(maybe, just(hi)).
is_of_type(maybe, just(93)).
is_of_type(maybe, just(42.7)).
is_of_type(maybe, just([a,b(c),d])).


% with inner type description
is_of_type(maybe(integer), nothing).
is_of_type(maybe(integer), just(-3)).

is_of_type(maybe(atom), nothing).
is_of_type(maybe(atom), just(dog)).

is_of_type(maybe(list), nothing).
is_of_type(maybe(list), just([])).
is_of_type(maybe(list), just([1,2,3])).

is_of_type(maybe(list(atom)), nothing).
is_of_type(maybe(list(atom)), just([])).
is_of_type(maybe(list(atom)), just([a,b,c,d])).
