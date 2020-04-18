module List.NonEmpty.Zipper exposing (..)

import List.NonEmpty as NE exposing (NonEmptyList)


{-| Zipper is opaque type because it
copntains some internal semantic which we don't want to leak to a user
-}
type Zipper a
    = Zipper (List a) a (List a)


singleton : a -> Zipper a
singleton a =
    Zipper [] a []


fromNonEmpty : NonEmptyList a -> Zipper a
fromNonEmpty ( h, t ) =
    Zipper [] h t


fromList : List a -> Maybe (Zipper a)
fromList =
    Maybe.map fromNonEmpty << NE.fromList


fromCons : a -> List a -> Zipper a
fromCons a =
    fromNonEmpty << NE.fromCons a


custom : List a -> a -> List a -> Zipper a
custom p f n =
    Zipper p f n


toNonEmpty : Zipper a -> NonEmptyList a
toNonEmpty (Zipper p f n) =
    case List.reverse p of
        [] ->
            ( f, n )

        h :: t ->
            ( h, t ++ f :: n )


toList : Zipper a -> List a
toList =
    NE.toList << toNonEmpty



-- Query


current : Zipper a -> a
current (Zipper _ focus _) =
    focus


listNext : Zipper a -> List a
listNext (Zipper _ _ n) =
    n


listPrev : Zipper a -> List a
listPrev (Zipper p _ _) =
    List.reverse p


hasNext : Zipper a -> Bool
hasNext (Zipper _ _ n) =
    not <| List.isEmpty n


hasPrev : Zipper a -> Bool
hasPrev (Zipper p _ _) =
    not <| List.isEmpty p


length : Zipper a -> Int
length (Zipper p _ n) =
    List.length p + List.length n + 1



-- Movement


next : Zipper a -> Maybe (Zipper a)
next (Zipper p f n) =
    case n of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper (f :: p) h t


prev : Zipper a -> Maybe (Zipper a)
prev (Zipper p f n) =
    case p of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper t h <| f :: n


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
forward (Zipper p f n) =
    case n of
        [] ->
            case List.reverse <| f :: p of
                -- singleton zipper
                [] ->
                    Zipper p f n

                h :: t ->
                    Zipper [] h t

        h :: t ->
            Zipper (f :: p) h t


backward : Zipper a -> Zipper a
backward (Zipper p f n) =
    case p of
        [] ->
            case List.reverse <| f :: n of
                -- singleton zipper
                [] ->
                    Zipper p f n

                h :: t ->
                    Zipper t h []

        h :: t ->
            Zipper t h <| f :: n


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



-- Functor


map : (a -> b) -> Zipper a -> Zipper b
map fc (Zipper p f n) =
    Zipper (List.map fc p) (fc f) <|
        List.map fc n



-- Foldable


foldl : (a -> b -> b) -> b -> Zipper a -> b
foldl f acc =
    NE.foldl f acc << toNonEmpty


foldl1 : (a -> a -> a) -> Zipper a -> a
foldl1 f =
    NE.foldr1 f << toNonEmpty


foldr : (a -> b -> b) -> b -> Zipper a -> b
foldr f acc =
    NE.foldr f acc << toNonEmpty


foldr1 : (a -> a -> a) -> Zipper a -> a
foldr1 f =
    NE.foldr1 f << toNonEmpty



-- Comonad


duplicate : Zipper a -> Zipper (Zipper a)
duplicate =
    genericMove prev next


extend : (Zipper a -> b) -> Zipper a -> Zipper b
extend f =
    map f << duplicate


maybeIter : (a -> Maybe a) -> List a -> a -> List a
maybeIter f acc a =
    case f a of
        Just val ->
            maybeIter f (val :: acc) val

        Nothing ->
            List.reverse acc


genericMove : (a -> Maybe a) -> (a -> Maybe a) -> a -> Zipper a
genericMove f g z =
    Zipper (maybeIter f [] z) z (maybeIter g [] z)
