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
toNonEmpty (Zipper r) =
    case List.reverse r.prev of
        [] ->
            ( r.focus, r.next )

        h :: _ ->
            ( h, r.focus :: r.next )


toList : Zipper a -> List a
toList =
    NE.toList << toNonEmpty



-- Query


current : Zipper a -> a
current (Zipper { focus }) =
    focus


listNext : Zipper a -> List a
listNext (Zipper r) =
    r.next


listPrev : Zipper a -> List a
listPrev (Zipper r) =
    List.reverse r.prev


hasNext : Zipper a -> Bool
hasNext (Zipper r) =
    case r.next of
        [] ->
            False

        _ ->
            True


hasPrev : Zipper a -> Bool
hasPrev (Zipper r) =
    case r.prev of
        [] ->
            False

        _ ->
            True


length : Zipper a -> Int
length (Zipper r) =
    List.length r.prev + List.length r.next + 1



-- Movement


next : Zipper a -> Maybe (Zipper a)
next (Zipper r) =
    case r.next of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper { prev = r.focus :: r.prev, focus = h, next = t }


prev : Zipper a -> Maybe (Zipper a)
prev (Zipper r) =
    case r.prev of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper { prev = t, focus = h, next = r.focus :: r.next }


attemptNext : Zipper a -> Zipper a
attemptNext zipper =
    Maybe.withDefault zipper <| next zipper


attemptPrev : Zipper a -> Zipper a
attemptPrev zipper =
    Maybe.withDefault zipper <| prev zipper


nextBy : Int -> Zipper a -> Maybe (Zipper a)
nextBy =
    byMaybeHelper next


prevBy : Int -> Zipper a -> Maybe (Zipper a)
prevBy =
    byMaybeHelper prev


byMaybeHelper : (Zipper a -> Maybe (Zipper a)) -> Int -> Zipper a -> Maybe (Zipper a)
byMaybeHelper step n acc =
    if n < 1 then
        Just acc

    else
        case step acc of
            Just newAcc ->
                byMaybeHelper step (n - 1) newAcc

            Nothing ->
                Nothing


attemptNextBy : Int -> Zipper a -> Zipper a
attemptNextBy =
    attemptByHelper next


attemptPrevBy : Int -> Zipper a -> Zipper a
attemptPrevBy =
    attemptByHelper prev


attemptByHelper : (Zipper a -> Maybe (Zipper a)) -> Int -> Zipper a -> Zipper a
attemptByHelper step n acc =
    if n < 1 then
        acc

    else
        case step acc of
            Just newAcc ->
                attemptByHelper step (n - 1) newAcc

            Nothing ->
                acc



-- Cycling


forward : Zipper a -> Zipper a
forward (Zipper r) =
    case r.next of
        [] ->
            case List.reverse <| r.focus :: r.prev of
                -- singleton zipper
                [] ->
                    Zipper r

                h :: t ->
                    Zipper { prev = [], focus = h, next = t }

        h :: t ->
            Zipper { prev = r.focus :: r.prev, focus = h, next = t }


backward : Zipper a -> Zipper a
backward (Zipper r) =
    case r.prev of
        [] ->
            case List.reverse <| r.focus :: r.next of
                -- singleton zipper
                [] ->
                    Zipper r

                h :: t ->
                    Zipper { prev = t, focus = h, next = [] }

        h :: t ->
            Zipper { prev = t, focus = h, next = r.focus :: r.next }


forwardBy : Int -> Zipper a -> Zipper a
forwardBy =
    rewindByHelper forward


backwardBy : Int -> Zipper a -> Zipper a
backwardBy =
    rewindByHelper backward


rewindByHelper : (Zipper a -> Zipper a) -> Int -> Zipper a -> Zipper a
rewindByHelper step n acc =
    if n < 1 then
        acc

    else
        rewindByHelper step (n - 1) <| step acc
