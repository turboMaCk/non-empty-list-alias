module Maybe.NonEmpty exposing (sequenceNe)

{-| Extensions to `Maybe` and `Maybe.Extra` modules
provifing functions to work with `List.NoneEmpty.NonEmpty` type.

It's safe to import this module as Maybe:

    import Maybe.NonEmpty as Maybe


# Traverse

@docs sequenceNe

-}

import List.NonEmpty as NonEmpty exposing (NonEmpty)


{-| If every `Maybe` in the list is present, return all of the values unwrapped.
If there are any `Nothing`s, the whole function fails and returns `Nothing`.

    sequence (Just 1, [])
    --> Just (1, [])

    sequence (Just 1, [Just 2, Just 3])
    --> Just (1, [2, 3])

    sequence (Just 1,  [Nothing, Just 3])
    --> Nothing

-}
sequenceNe : NonEmpty (Maybe a) -> Maybe (NonEmpty a)
sequenceNe ( m, ms ) =
    Maybe.map2 NonEmpty.fromCons m (List.foldr (Maybe.map2 (::)) (Just []) ms)
