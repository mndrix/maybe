# Synopsis

    :- use_module(library(maybe)).
    % staff(Name, Spouse)
    staff(tom, just(teresa)).
    staff(bob, nothing).
    staff(sue, just(william)).

    ?- staff(Name, MaybeSpouse),
       maybe_default_value(MaybeSpouse, '(none)', Spouse).
     Name = tom,
     Spouse = teresa ;
     Name = bob,
     Spouse = '(none)' ;
     Name = sue,
     Spouse = william.

# Description

The `maybe` type encapsulates an optional value.  When a value is present, we have `just(Value)`.  When it's absent we have `nothing`.  In some circumstances, this can be a more natural model than using Prolog failure.  For example, one might model a nullable SQL column using `maybe`.

This module draws inspiration from similar libraries for [Mercury](http://www.mercurylang.org/information/doc-release/mercury_library/maybe.html#maybe) and [Haskell](http://hackage.haskell.org/package/base/docs/Data-Maybe.html).  We make predicates available even where it's clearer to use unification.  That facilitates using maybe values with `maplist` and friends.

In addition to the predicates described below, this module also defines clauses for the multifile predicate `error:has_type/2` which describe the type `maybe` and `maybe(T)` where `T` is a type parameter.

# Changes in this Version

  * Initial public release

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(maybe).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/maybe

@author Michael Hendricks <michael@ndrix.org>
@license BSD
