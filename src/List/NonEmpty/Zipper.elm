module List.NonEmpty.Zipper exposing
    ( Zipper, singleton, fromNonEmpty, fromList, fromCons, custom
    , toList, toNonEmpty
    )

{-|

@docs Zipper, singleton, fromNonEmpty, fromList, fromCons, fromConsList, custom


## Convert

@docs toNonEmpty toList


## Change


### Insert without chaning focus

@docs insertBefore, insertAfter


### Insert and change focus

@docs consBefore, consAfter

-}

import List.NonEmpty as NE exposing (NonEmpty)


{-| Zipper is opaque type because it
copntains some internal semantic which we don't want to leak to a user
-}
type Zipper a
    = Zipper (List a) a (List a)


singleton : a -> Zipper a
singleton a =
    Zipper [] a []


fromNonEmpty : NonEmpty a -> Zipper a
fromNonEmpty ( h, t ) =
    Zipper [] h t


fromList : List a -> Maybe (Zipper a)
fromList =
    Maybe.map fromNonEmpty << NE.fromList


fromCons : a -> List a -> Zipper a
fromCons a =
    fromNonEmpty << NE.fromCons a


fromConsList : List a -> NonEmpty a -> Zipper a
fromConsList p ( f, n ) =
    Zipper (List.reverse p) f n


custom : List a -> a -> List a -> Zipper a
custom p f n =
    Zipper (List.reverse p) f n


toNonEmpty : Zipper a -> NonEmpty a
toNonEmpty (Zipper p f n) =
    case List.reverse p of
        [] ->
            ( f, n )

        h :: t ->
            ( h, t ++ f :: n )


toList : Zipper a -> List a
toList =
    NE.toList << toNonEmpty


inserBefore : a -> Zipper a -> Zipper a
inserBefore a (Zipper b f n) =
    Zipper (a :: b) f n


inserAfter : a -> Zipper a -> Zipper a
inserAfter a (Zipper b f n) =
    Zipper b f (a :: n)


consBefore : a -> Zipper a -> Zipper a
consBefore a (Zipper b f n) =
    Zipper b a (f :: n)


consAfter : a -> Zipper a -> Zipper a
consAfter a (Zipper b f n) =
    Zipper (f :: b) a n



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



-- Ends


start : Zipper a -> Zipper a
start =
    toEndHelper prev


end : Zipper a -> Zipper a
end =
    toEndHelper next


toEndHelper : (a -> Maybe a) -> a -> a
toEndHelper f acc =
    case f acc of
        Just val ->
            toEndHelper f val

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


relativeIndexedMap : (Int -> a -> b) -> Zipper a -> Zipper b
relativeIndexedMap f (Zipper p focus n) =
    Zipper (List.indexedMap (\i -> f (-1 * (1 + i))) p) (f 0 focus) <|
        List.indexedMap (\i -> f (i + 1)) n


absoluteIndexedMap : (Int -> a -> b) -> Zipper a -> Zipper b
absoluteIndexedMap f (Zipper p focus n) =
    let
        prevLength =
            List.length p
    in
    Zipper (List.indexedMap (\i -> f (prevLength - 1 - i)) p) (f prevLength focus) <|
        List.indexedMap (\i -> f (prevLength + 1 + i)) n



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



-- Applicative


map2 : (a -> b -> c) -> Zipper a -> Zipper b -> Zipper c
map2 f (Zipper p1 f1 n1) (Zipper p2 f2 n2) =
    Zipper (List.map2 f p1 p2) (f f1 f2) (List.map2 f n1 n2)


andMap : Zipper a -> Zipper (a -> b) -> Zipper b
andMap =
    map2 (|>)



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


duplicateList : Zipper a -> List (Zipper a)
duplicateList =
    toList << duplicate


genericMove : (a -> Maybe a) -> (a -> Maybe a) -> a -> Zipper a
genericMove f g z =
    Zipper (maybeIter f [] z) z (maybeIter g [] z)
