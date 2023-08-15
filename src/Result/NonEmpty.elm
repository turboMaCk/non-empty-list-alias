module Result.NonEmpty exposing (combineNe, combineMapNe)

{-| Extensions to `Result` and `Result.Extra` modules
providing functions to work with `List.NoneEmpty.NonEmpty`.

It's safe to import this module as Result:

    import Result.NonEmpty as Result

all functions in this module use `Ne` suffix to prevent collisions with
`List` based alternatives.


# combineMap

@docs combineNe, combineMapNe

-}

import List.NonEmpty exposing (NonEmpty)


{-| If every `Result` in the nonempty list is `Ok`, return all of the values unwrapped.
If there are any `Err`s, return the first `Err` instead.

    combineNe ( Ok 1, [] )
    --> Ok (1, [])

    combineNe ( Ok 1, [ Ok 2, Ok 3 ] )
    --> Ok ( 1, [ 2, 3 ] )

    combineNe ( Ok 1, [ Err "error 1", Ok 3, Err "error 2" ] )
    --> Err "error 1"

-}
combineNe : NonEmpty (Result x a) -> Result x (NonEmpty a)
combineNe ( head, tail ) =
    case head of
        Err e ->
            Err e

        Ok v ->
            combineMapNeHelp identity ( v, [] ) tail


{-| Like [`combineNe`](#combineNe), but map a function over each element of the list first.

If every function call succeeds (returns `Just`), `combineMapNe` will return a non empty list.
If any function call fails (returns `Nothing`), `combineMap` will return `Nothing`.

`combineNe` is equivalent to `combineMapNe identity`.

    combineMapNe (\x -> Ok (x * 10)) ( 1, [ 2, 3, 4, 5 ] )
    --> Ok ( 10, [ 20, 30, 40, 50 ] )

    combineMapNe identity ( Ok 1, [ Ok 2, Err "error 1", Err "error 2" ] )
    --> Err "error 1"

-}
combineMapNe : (a -> Result x b) -> NonEmpty a -> Result x (NonEmpty b)
combineMapNe f ( head, tail ) =
    case f head of
        Err e ->
            Err e

        Ok v ->
            combineMapNeHelp f ( v, [] ) tail


combineMapNeHelp : (a -> Result x b) -> NonEmpty b -> List a -> Result x (NonEmpty b)
combineMapNeHelp f ( head, tail ) xs =
    case xs of
        [] ->
            Ok ( head, List.reverse tail )

        h :: t ->
            case f h of
                Ok v ->
                    combineMapNeHelp f ( head, v :: tail ) t

                Err e ->
                    Err e
