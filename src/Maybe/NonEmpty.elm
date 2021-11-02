module Maybe.NonEmpty exposing (sequenceNe, combineNe, traverseNe)

{-| Extensions to `Maybe` and `Maybe.Extra` modules
provifing functions to work with `List.NoneEmpty.NonEmpty` type.

It's safe to import this module as Maybe:

    import Maybe.NonEmpty as Maybe

all functins in this module use `Ne` suffix to prevent collising with
`List` based alternatives.


# Traverse

@docs sequenceNe, combineNe, traverseNe

-}

import List.NonEmpty as NonEmpty exposing (NonEmpty)


{-| If every `Maybe` in the list is present, return all of the values unwrapped.
If there are any `Nothing`s, the whole function fails and returns `Nothing`.

    sequenceNe ( Just 1, [] )
    --> Just (1, [])

    sequenceNe ( Just 1, [ Just 2, Just 3 ] )
    --> Just (1, [2, 3])

    sequenceNe ( Just 1,  [ Nothing, Just 3 ] )
    --> Nothing

-}
sequenceNe : NonEmpty (Maybe a) -> Maybe (NonEmpty a)
sequenceNe ( m, ms ) =
    Maybe.map2 NonEmpty.fromCons m (List.foldr (Maybe.map2 (::)) (Just []) ms)


{-| If every `Maybe` in the none emptu list is present, return all of the values unwrapped.
If there are any `Nothing`s, the whole function fails and returns `Nothing`.

    combineNe ( Just 1, [ Just 2, Just 3 ] )
    --> Just ( 1, [ 2, 3 ] )

    combineNe ( Just 1, [ Nothing, Just 3 ] )
    --> Nothing

-}
combineNe : NonEmpty (Maybe a) -> Maybe (NonEmpty a)
combineNe ( head, tail ) =
    case head of
        Nothing ->
            Nothing

        Just v ->
            traverseNeHelp identity ( v, [] ) tail



{-| Like [`combineNe`](#combineNe), but map a function over each element of the list first.

If every function call succeeds (returns `Just`), `traverseNe` will return a non empty list.
If any function call fails (returns `Nothing`), `traverse` will return `Nothing`.

`combineNe` is equivalent to `traverseNe identity`.

    traverseNe (\x -> Just (x * 10)) ( 1, [ 2, 3, 4, 5 ] )
    --> Just ( 10, [ 20, 30, 40, 50 ] )

    traverseNe List.head ( [1], [ [2, 3], [] ] )
    --> Nothing

-}
traverseNe : (a -> Maybe b) -> NonEmpty a -> Maybe (NonEmpty b)
traverseNe f ( head, tail ) =
    case f head of
        Nothing ->
            Nothing

        Just v ->
            traverseNeHelp f ( v, [] ) tail


traverseNeHelp : (a -> Maybe b) -> NonEmpty b -> List a -> Maybe (NonEmpty b)
traverseNeHelp f ( head, tail ) xs =
    case xs of
        [] ->
            Just ( head, List.reverse tail )

        h :: t ->
            case f h of
                Just v ->
                    traverseNeHelp f ( head, v :: tail ) t

                Nothing ->
                    Nothing
