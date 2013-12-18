:- module(maybe, [ is_just/1
                 , is_nothing/1
                 , just_value/2
                 , maybe_value/2
                 , maybe_list/2
                 , maybe_default_value/3
                 , default_maybe_value/3
                 , map_maybe/3
                 , fold_maybe/4
                 ]).
:- use_module(library(error), [ domain_error/2 ]).


:- multifile error:has_type/2.
error:has_type(maybe, Maybe) :-
    nonvar(Maybe),
    memberchk(Maybe, [nothing, just(_)]).
error:has_type(maybe(T), Maybe) :-
    error:has_type(maybe, Maybe),
    ( Maybe = just(X) ->
        error:has_type(T, X)
    ; true
    ).

% we don't load library(quickcheck) to avoid a dependency
:- multifile quickcheck:arbitrary/2.
quickcheck:arbitrary(maybe, Maybe) :-
    quickcheck:arbitrary(maybe(any), Maybe).
quickcheck:arbitrary(maybe(T), Maybe) :-
    random_between(0, 4, N),
    ( N == 0 ->
        Maybe = nothing
    ; % otherwise ->
        quickcheck:arbitrary(T, X),
        Maybe = just(X)
    ).

:- multifile quickcheck:shrink/3.
quickcheck:shrink(maybe(T), just(X0), just(X)) :-
    quickcheck:shrink(T, X0, X).


%% is_just(+Maybe:maybe) is semidet.
%
%  True if Maybe is `just(_)`. Useful for `include/3`.
is_just(just(_)).


%% is_nothing(+Maybe:maybe) is semidet.
%
%  True if Maybe is `nothing`.  Useful for `exclude/3`.
is_nothing(nothing).


%% just_value(+Just:maybe(T), -Value:T) is det.
%% just_value(-Just:maybe(T), +Value:T) is det.
%
%  True if Just wraps Value. Throws an exception if Just is
%  `nothing`.
just_value(just(Value), Value) :-
    !.
just_value(nothing, _) :-
    domain_error(just, nothing).


%% maybe_value(+Maybe:maybe(T), -Value:T) is det.
%
%  True if Maybe is `just(Value)`; fails for `nothing`.
maybe_value(just(Value), Value).


%% maybe_list(?Maybe:maybe(T), ?List:list(T)) is det.
%
%  Relates a List to a Maybe value. An empty list is equivalent to
%  `nothing`.  A non-empty list is equivalent to `just(Head)`.
maybe_list(nothing, []).
maybe_list(just(H), [H|_]).


%% maybe_default_value(+Maybe:maybe(T), +Default:T, -Value:T) is det.
%% maybe_default_value(-Maybe:maybe(T), +Default:T, +Value:T) is multi.
%
%  True if Maybe wraps Value with a Default for the `nothing` case.
maybe_default_value(nothing, Default, Default).
maybe_default_value(just(Value), _, Value).

%% default_maybe_value(+Default:T, +Maybe:maybe(T), -Value:T) is det.
%% default_maybe_value(+Default:T, -Maybe:maybe(T), +Value:T) is multi.
%
%  Like maybe_default_value/3 with different argument order. This
%  argument order is convenient for maplist/3 like so:
%
%      maplist(default_maybe_value(7), Maybes, DefaultedValues).
default_maybe_value(Default, Maybe, Value) :-
    maybe_default_value(Maybe, Default, Value).


%% map_maybe(+Goal, ?Maybe0:maybe, ?Maybe:maybe)
%
%  True if `call(Goal, Value0, Value)` succeeds for `just` values. Goal
%  is not called for `nothing` values, which remain unchanged. "Use the
%  source" for a clearer explanation.
map_maybe(_, nothing, nothing) :-
    !.  % help indexer
map_maybe(Goal, just(V0), just(V)) :-
    call(Goal, V0, V).


%% fold_maybe(+Goal, ?Maybe:maybe, ?Accum0, ?Accum)
%
%  `nothing` leaves `Accum0=Accum` while `just` relates them via Goal.
%  "Use the source" for a clearer explanation.
fold_maybe(_, nothing, Accum, Accum).
fold_maybe(Goal, just(Value), Accum0, Accum) :-
    call(Goal, Value, Accum0, Accum).
