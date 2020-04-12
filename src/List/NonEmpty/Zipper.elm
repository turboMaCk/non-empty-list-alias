module List.NonEmpty.Zipper exposing (..)

import List.NonEmpty as NE exposing (NonEmptyList)


{-| Zipper is opaque type because it
copntains some internal semantic which we don't want to leak to a user
-}
type Zipper a
    = Zipper
        { prev : List a
        , focus : a
        , next : List a
        }


singleton : a -> Zipper a
singleton a =
    Zipper { prev = [], focus = a, next = [] }


fromNonEmpty : NonEmptyList a -> Zipper a
fromNonEmpty ( h, t ) =
    Zipper { prev = [], focus = h, next = t }


fromList : List a -> Maybe (Zipper a)
fromList =
    Maybe.map fromNonEmpty << NE.fromList


fromCons : a -> List a -> Zipper a
fromCons a =
    fromNonEmpty << NE.fromCons a


toNonEmpty : Zipper a -> NonEmptyList a
toNonEmpty (Zipper { prev, focus, next }) =
    case List.reverse prev of
        [] ->
            ( focus, next )

        h :: _ ->
            ( h, focus :: next )


toList : Zipper a -> List a
toList =
    NE.toList << toNonEmpty



-- Query


current : Zipper a -> a
current (Zipper { focus }) =
    focus


listNext : Zipper a -> List a
listNext (Zipper { next }) =
    next


listPrev : Zipper a -> List a
listPrev (Zipper { prev }) =
    List.reverse prev


hasNext : Zipper a -> Bool
hasNext (Zipper { next }) =
    case next of
        [] ->
            False

        _ ->
            True


hasPrev : Zipper a -> Bool
hasPrev (Zipper { prev }) =
    case prev of
        [] ->
            False

        _ ->
            True


length : Zipper a -> Int
length (Zipper { prev, next }) =
    List.length prev + List.lengt next + 1



-- Movement
