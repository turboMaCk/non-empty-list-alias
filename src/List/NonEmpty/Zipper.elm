module List.NonEmpty.Zipper exposing
    ( Zipper, singleton, fromNonEmpty, fromList, fromCons, fromConsList, custom
    , current, listPrev, listNext, hasPrev, hasNext, length
    , insertBefore, insertAfter
    , consBefore, consAfter
    , next, prev, nextBy, prevBy
    , attemptNext, attemptPrev, attemptPrevBy, attemptNextBy
    , start, end
    , forward, backward, forwardBy, backwardBy
    , map, relativeIndexedMap, absoluteIndexedMap, foldl, foldr, foldl1, foldr1
    , map2, andMap
    , duplicate, extend, duplicateList
    , toNonEmpty, toList
    )

{-|

@docs Zipper, singleton, fromNonEmpty, fromList, fromCons, fromConsList, custom


## Query

Functions that query `Zipper` for additional data.

@docs current, listPrev, listNext, hasPrev, hasNext, length


# Insert new values

Following function inserts new value into existing `Zipper`.


## Insert without chaining focus

This function insert value around focus without moving it.

@docs insertBefore, insertAfter


## Insert and change focus

These functions insert value around focus while moving focus on newly inserted value.

@docs consBefore, consAfter


# Movement

Functions that move focus within `Zipper` around without losing data.


## Bounded Movement

These function will return `Nothing` when moving out of bounds of `Zipper`.

@docs next, prev, nextBy, prevBy


## Direction Movement

These function will move in direction but won't reach out of bound.
When end on any side is reached, the last value on this side is returned.

@docs attemptNext, attemptPrev, attemptPrevBy, attemptNextBy


## Bounds

These helper function will move from either side of a `Zipper`

@docs start, end


## Cycling Movement

These function move in cycles around the zipper. Value on a very start is preceded by
value in the end. These function simply move in circle and never reach the end of a `Zipper`.

@docs forward, backward, forwardBy, backwardBy


# Transform

@docs map, relativeIndexedMap, absoluteIndexedMap, foldl, foldr, foldl1, foldr1


# Combine

@docs map2, andMap


# Expand

@docs duplicate, extend, duplicateList


# Convert

@docs toNonEmpty, toList

-}

import List.NonEmpty as NE exposing (NonEmpty)


{-| Zipper type.

This can be thought of as `NonEmpty` which holds keeps track
of unconsed data.

Unlike `NonEmpty` this type is opaque as it needs to ensure
internal invariants.

-}
type Zipper a
    = Zipper (List a) a (List a)


{-| Put single value into a `Zipper`.

    singleton "foo"
    |> current
    --> "foo"

-}
singleton : a -> Zipper a
singleton a =
    Zipper [] a []


{-| Init `Zipper` from `NonEmpty` list type.

    fromNonEmpty ( 1, [ 2, 3 ] )
    |> current
    --> 1

    fromNonEmpty ( 1, [ 2, 3 ] )
    |> toList
    --> [ 1, 2, 3 ]

-}
fromNonEmpty : NonEmpty a -> Zipper a
fromNonEmpty ( h, t ) =
    Zipper [] h t


{-| Init `Zipper` from `List`.
This operation is not successful for `[]`

    fromList []
    --> Nothing

    fromList [1, 2, 3]
    --> Just (custom [] 1 [2,3])

-}
fromList : List a -> Maybe (Zipper a)
fromList =
    Maybe.map fromNonEmpty << NE.fromList


{-| Init `Zipper` by consing value onto the list.

    fromCons 1 [ 2, 3 ]
    |> current
    --> 1

-}
fromCons : a -> List a -> Zipper a
fromCons a =
    fromNonEmpty << NE.fromCons a


{-| Init `Zipper` by consing `List` onto `NonEmpty`.

The head of NonEmpty stays in focus while list is a list
of previous heads.

    fromConsList [] (1, [2])
    |> current
    --> 1

    fromConsList [1, 2] (3, [4])
    |> prev
    |> Maybe.map current
    --> Just 2

-}
fromConsList : List a -> NonEmpty a -> Zipper a
fromConsList p ( f, n ) =
    Zipper (List.reverse p) f n


{-| Init `Zipper` from parts.

    custom [1,2] 3 [4,5]
    |> current
    --> 3

    custom [1,2] 3 [4,5]
    |> prev
    |> Maybe.map current
    --> Just 2

-}
custom : List a -> a -> List a -> Zipper a
custom p f n =
    Zipper (List.reverse p) f n


{-| Convert `Zipper` back to `NonEmpty`.

This function won't loose data, all previous heads are added back.

    fromCons 1 [2,3]
    |> toNonEmpty
    --> (1, [2, 3])


    fromConsList [1,2] (3, [4])
    |> toNonEmpty
    --> (1, [2,3,4])

-}
toNonEmpty : Zipper a -> NonEmpty a
toNonEmpty (Zipper p f n) =
    case List.reverse p of
        [] ->
            ( f, n )

        h :: t ->
            ( h, t ++ f :: n )


{-| Convert `Zipper` to `List`.

    singleton 1
    |> toList
    --> [1]

    custom [1,2] 3 []
    |> toList
    --> [1,2,3]

-}
toList : Zipper a -> List a
toList =
    NE.toList << toNonEmpty


{-| Insert new value before current focus.

    fromConsList [1, 2] (4, [5])
    |> insertBefore 3
    |> toList
    --> [1,2,3,4,5]

    fromConsList [1, 2] (4, [5])
    |> insertBefore 3
    |> current
    --> 4

-}
insertBefore : a -> Zipper a -> Zipper a
insertBefore a (Zipper b f n) =
    Zipper (a :: b) f n


{-| Insert new value after current focus.

    fromConsList [1, 2] (3, [5])
    |> insertAfter 4
    |> toList
    --> [1,2,3,4,5]

    fromConsList [1, 2] (3, [5])
    |> insertAfter 4
    |> current
    --> 3

-}
insertAfter : a -> Zipper a -> Zipper a
insertAfter a (Zipper b f n) =
    Zipper b f (a :: n)


{-| Insert value before current focus and move focus to it.

    fromConsList [1, 2] (4, [5])
    |> consBefore 3
    |> toList
    --> [1,2,3,4,5]

    fromConsList [1, 2] (4, [5])
    |> consBefore 3
    |> current
    --> 3

-}
consBefore : a -> Zipper a -> Zipper a
consBefore a (Zipper b f n) =
    Zipper b a (f :: n)


{-| Insert new value after current focus and move focus to it.

    fromConsList [1, 2] (3, [5])
    |> consAfter 4
    |> toList
    --> [1,2,3,4,5]

    fromConsList [1, 2] (3, [5])
    |> consAfter 4
    |> current
    --> 4

-}
consAfter : a -> Zipper a -> Zipper a
consAfter a (Zipper b f n) =
    Zipper (f :: b) a n



-- Query


{-| Get current focus

    custom [1,2] 3 [4,5]
    |> current
    --> 3

-}
current : Zipper a -> a
current (Zipper _ focus _) =
    focus


{-| Get `List` of all values following current focus.

    custom [1,2] 3 [4,5]
    |> listNext
    --> [4,5]

-}
listNext : Zipper a -> List a
listNext (Zipper _ _ n) =
    n


{-| Get `List` of all values preceding current focus.

    custom [1,2] 3 [4,5]
    |> listPrev
    --> [1,2]

-}
listPrev : Zipper a -> List a
listPrev (Zipper p _ _) =
    List.reverse p


{-| Check if there is next value after current focus.

    custom [1,2] 3 [4,5]
    |> hasNext
    --> True

    custom [1,2] 3 []
    |> hasNext
    --> False

-}
hasNext : Zipper a -> Bool
hasNext (Zipper _ _ n) =
    not <| List.isEmpty n


{-| Check if there is next value before current focus.

    custom [1,2] 3 [4,5]
    |> hasPrev
    --> True

    custom [] 1 [2,3]
    |> hasPrev
    --> False

-}
hasPrev : Zipper a -> Bool
hasPrev (Zipper p _ _) =
    not <| List.isEmpty p


{-| Get length of Zipper

    custom [1,2] 3 [4]
    |> length
    --> 4

-}
length : Zipper a -> Int
length (Zipper p _ n) =
    List.length p + List.length n + 1



-- Movement


{-| Move focus to next value.

    custom [] 1 [2,3]
    |> next
    |> Maybe.map current
    |> Just 2

    custom [] 1 []
    |> next
    --> Nothing

-}
next : Zipper a -> Maybe (Zipper a)
next (Zipper p f n) =
    case n of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper (f :: p) h t


{-| Move focus to next value.

    custom [1, 2] 3 []
    |> prev
    |> Maybe.map current
    |> Just 2

    custom [] 1 []
    |> prev
    --> Nothing

-}
prev : Zipper a -> Maybe (Zipper a)
prev (Zipper p f n) =
    case p of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper t h <| f :: n


{-| Move focus to next value if such value exists.

    custom [] 1 [2,3]
    |> attemptNext
    |> current
    |> 2

    custom [] 1 []
    |> attemptNext
    |> current
    --> 1

-}
attemptNext : Zipper a -> Zipper a
attemptNext zipper =
    Maybe.withDefault zipper <| next zipper


{-| Move focus to previous value if such value exists.

    custom [1] 2 [3]
    |> attemptPrev
    |> current
    |> 1

    custom [] 1 []
    |> attemptPrev
    |> current
    --> 1

-}
attemptPrev : Zipper a -> Zipper a
attemptPrev zipper =
    Maybe.withDefault zipper <| prev zipper


{-| Perform [`next`](#next) n times.
-}
nextBy : Int -> Zipper a -> Maybe (Zipper a)
nextBy =
    byMaybeHelper next


{-| Perform [`prev`](#prev) n times.
-}
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


{-| Perform [`attemptNext`](#attemptNext) n times.
-}
attemptNextBy : Int -> Zipper a -> Zipper a
attemptNextBy =
    attemptByHelper next


{-| Perform [`attemptPrev`](#attemptPrev) n times.
-}
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


{-| Move focus to the very first value

    custom [ 1, 2, 3 ] 4 [ 5, 6, 7 ]
    |> start
    |> current
    --> 1

-}
start : Zipper a -> Zipper a
start =
    toEndHelper prev


{-| Move focus to the very last value

    custom [ 1, 2, 3 ] 4 [ 5, 6, 7 ]
    |> end
    |> current
    --> 7

-}
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


{-| Move focus to next value, go back to fist value if current value is last.

    custom [] 1 [2,3]
    |> forward
    |> current
    |> 2

    custom [1,2] 3 []
    |> forward
    |> current
    --> 1

-}
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


{-| Move focus to previous value, go to last value if current value is fist one.

    custom [1, 2] 3 []
    |> backward
    |> current
    |> 2

    custom [] 1 [2,3]
    |> backward
    |> current
    --> 3

-}
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


{-| Move [`forward`](#forward) n times.
-}
forwardBy : Int -> Zipper a -> Zipper a
forwardBy =
    rewindByHelper forward


{-| Move [`backward`](#backward) n times.
-}
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


{-| Map a function over Zipper

    map String.fromInt (custom [1] 2 [3, 4])
    |> toList
    --> ["1", "2", "3", "4"]

-}
map : (a -> b) -> Zipper a -> Zipper b
map fc (Zipper p f n) =
    Zipper (List.map fc p) (fc f) <|
        List.map fc n


{-| Indexed map relative to the position in the zipper.

    custom ["a", "b"] "c" ["d"]
    |> relativeIndexedMap (\index el -> (index, el))
    |> toList
    --> [(-2,"a"),(-1,"b"), (0,"c"), (1,"d")]

-}
relativeIndexedMap : (Int -> a -> b) -> Zipper a -> Zipper b
relativeIndexedMap f (Zipper p focus n) =
    Zipper (List.indexedMap (\i -> f (-1 * (1 + i))) p) (f 0 focus) <|
        List.indexedMap (\i -> f (i + 1)) n


{-| Indexed map. Starting with 0 from the beginning of the zipper

    custom ["a", "b"] "c" ["d"]
    |> absoluteIndexedMap (\index el -> (index, el))
    |> toList
    --> [(0,"a"),(1,"b"), (2,"c"), (3,"d")]

-}
absoluteIndexedMap : (Int -> a -> b) -> Zipper a -> Zipper b
absoluteIndexedMap f (Zipper p focus n) =
    let
        prevLength =
            List.length p
    in
    Zipper (List.indexedMap (\i -> f (prevLength - 1 - i)) p) (f prevLength focus) <|
        List.indexedMap (\i -> f (prevLength + 1 + i)) n



-- Foldable


{-| Reduce `Zipper` from left

    foldl (+) 0 <| custom [1,2] 3 [4]
    --> 10

-}
foldl : (a -> b -> b) -> b -> Zipper a -> b
foldl f acc =
    NE.foldl f acc << toNonEmpty


{-| Collapse `Zipper a` into `a` value from left

    foldl1 (++) <| custom ["hello"] " " ["world"]
    --> "world hello"

-}
foldl1 : (a -> a -> a) -> Zipper a -> a
foldl1 f =
    NE.foldl1 f << toNonEmpty


{-| Reduce `Zipper` from right

    foldr (+) 0 <| custom [1,2] 3 [4]
    --> 10

-}
foldr : (a -> b -> b) -> b -> Zipper a -> b
foldr f acc =
    NE.foldr f acc << toNonEmpty


{-| Collapse `Zipper a` into `a` value from right

    foldr1 (+) (custom [1,2] 3 [4])
    --> 10

    foldr1 (++) (custom ["hello"] " " ["world"])
    --> "hello world"

-}
foldr1 : (a -> a -> a) -> Zipper a -> a
foldr1 f =
    NE.foldr1 f << toNonEmpty



-- Applicative


{-| Combine two Zippers with a given function.
In case where one of the two zippers is longer the extra elements are ignored

    map2 (+) (custom [1] 2 []) (custom [1] 1 [])
    |> toList
    --> [2, 3]

    map2 (+) (custom [1] 2 [3]) (custom [1] 1 [])
    |> toList
    --> [2, 3]

-}
map2 : (a -> b -> c) -> Zipper a -> Zipper b -> Zipper c
map2 f (Zipper p1 f1 n1) (Zipper p2 f2 n2) =
    Zipper (List.map2 f p1 p2) (f f1 f2) (List.map2 f n1 n2)


{-| Map over multiple Zippers.

    map (+) (custom [1] 2 [3])
    |> andMap (custom [1] 2 [3])
    |> toList
    --> [2, 4, 6]

-}
andMap : Zipper a -> Zipper (a -> b) -> Zipper b
andMap =
    map2 (|>)



-- Comonad


{-| Create `Zipper` containing all possible variants of given Zipper.
Current version is focused one.

    custom [1] 2 [3]
    |> duplicate
    |> current
    --> custom [1] 2 [3]


    custom [1] 2 [3]
    |> duplicate
    |> forward
    |> current
    --> custom [1, 2] 3 []

-}
duplicate : Zipper a -> Zipper (Zipper a)
duplicate =
    genericMove prev next


{-| Map value to a new value based on surrounding structure.

This is a more advanced function following [`Comonad`](https://hackage.haskell.org/package/comonad)

    -- negate all True values which next value is not True itself
    fromNonEmpty ( True, [ True, True, False, True, True ] )
    |> extend (\zipper ->
                      let prev = current <| backward zipper
                      in prev && current zipper
                 )
    |> toNonEmpty
    --> (True, [True, True, False, False, True])

-}
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


{-| [`duplicate`](#duplicate) and covert to List.

This function might be useful in view code.

-}
duplicateList : Zipper a -> List (Zipper a)
duplicateList =
    toList << duplicate


genericMove : (a -> Maybe a) -> (a -> Maybe a) -> a -> Zipper a
genericMove f g z =
    Zipper (maybeIter f [] z) z (maybeIter g [] z)
