# List.NonEmpty alias with Zipper

[![Build Status](https://travis-ci.org/turboMaCk/non-empty-list-alias.svg?branch=master)](https://travis-ci.org/turboMaCk/non-empty-list-alias)

Functions for `NonEmpty` list you already have and `Zipper` implementation that goes with it.

Implementation of non-empty list defined as alias to pair `(a, List a)`.
This means that producing or consuming type compatible with this library doesn't require having it as a dependency.
Apart from this, this package aims to provide the most feature complete implementation of non-empty list for Elm and
includes variety of practical functions from JSON decoder helpers to hi-level combinators like `duplicate` and `extend`.

## Motivation

[elm/core](https://package.elm-lang.org/packages/elm/core/latest/) doesn't come with non empty list type.
This means one usually has to rely on library implementation of `NonEmpty` type:

* [mgold/elm-nonempty-list](https://package.elm-lang.org/packages/mgold/elm-nonempty-list/latest/)
* [hrldcpr/elm-cons](https://package.elm-lang.org/packages/hrldcpr/elm-cons/latest/)

These implementations usually define custom data type similar to `NonEmpty a = Cons a (List a)` and expose the constructor
to make pattern matching possible. This makes it hard for library authors to provide
support for `NonEmpty`, because they would need to pick one of these libraries and use it as a dependency
of their own implementation and essentially impose this decision onto their users.

This library takes different approach. `NonEmpty` is an alias for the pair `type alias NonEmpty a = (a, List a)`.
Relying on anonymous data-type like tuple means:

1. [Libraries](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#uncons) can produce `NonEmpty` data without depending on specific (including this) implementation.
1. Implementation provided by this package can be easily replaced by different implementation using the same type definition.
1. Users may choose to work with tuple directly without need to transform from and to `NonEmpty` type.

### Zipper

One of the downsides of common approach is also noticeable with available list zipper implementations.

* [jjant/elm-comonad-zipper](https://package.elm-lang.org/packages/jjant/elm-comonad-zipper/latest/)
* [wernerdegroot/listzipper](https://package.elm-lang.org/packages/wernerdegroot/listzipper/latest/)
* [yotamDvir/elm-pivot](https://package.elm-lang.org/packages/yotamDvir/elm-pivot/latest/Pivot)

All of above are usually constructed using `List a -> Maybe (Zipper a)` without possibility to construct zipper as `NonEmpty a -> Zipper a`.
The motivation behind including `Zipper` in this package is to encourage its usage together with `NonEmpty` list.
My favorite implementation of list zipper which doesn't rely on `NonEmpty` is [zwilias/elm-holey-zipper](https://package.elm-lang.org/packages/zwilias/elm-holey-zipper/latest).

### Drawbacks

Compared to "traditional" implementations this implementation has less descriptive constructor in value space.
This means that pattern matching happens on the pair instead of explicit constructor like `Cons` or `NonEmpty`.

**conventional library:**

```elm
matchNonEmpty : NonEmptyList a -> foo
matchNonEmpty (Cons h t) =
    .....
```

**this library:**

```elm
matchNonEmpty : NonEmptyList a -> foo
matchNonEmpty (h, t) =
    .....
```

## For Haskell Fanbois

`List.NoneEmpty.NonEmpty` is:

* Functor
* Foldable
* Applicative
* Monad
* Comonad

`List.NonEmpty.Zipper` is:

* Functor
* Foldable
* Applicative
* Comonad

`Maybe.NonEmpty` provides traversing functions between Maybes and `List.NonEmpty`.
