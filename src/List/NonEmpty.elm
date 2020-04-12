module List.NonEmpty exposing (..)


type alias NonEmptyList a =
    ( a, List a )


{-| Creates nonempty list with only one element.

    singleton 1
    --> ( 1, [] )

-}
singleton : a -> NonEmptyList a
singleton h =
    ( h, [] )


{-| Converts List to Maybe NonEmptyList

    fromList [ 1, 2 ]
    --> Just ( 1, [ 2 ] )

    fromList []
    --> Nothing

-}
fromList : List a -> Maybe (NonEmptyList a)
fromList xs =
    case xs of
        h :: t ->
            Just ( h, t )

        [] ->
            Nothing


{-| Cons element onto `List` to create `NoneEmptylist`

    fromCons 0 [ 1, 2 ]
    --> (0, [1, 2])

This function is just an alias on `Tuple.pair`

-}
fromCons : a -> List a -> NonEmptyList a
fromCons =
    Tuple.pair


{-| Converts NonEmptyList to List

    toList ( 1, [ 2 ] )
    --> [1, 2]

-}
toList : NonEmptyList a -> List a
toList ( h, t ) =
    h :: t


{-| Add element to the begining of `NonEmptylist`.

    cons 2 ( 1, [] )
    --> (2, [ 1 ])

-}
cons : a -> NonEmptyList a -> NonEmptyList a
cons a ( h, t ) =
    ( a, h :: t )


{-| Remove first element form `NonEmptylist`.

    uncons ( 3, [ 2, 1 ] )
    -> ( 3, Just ( 2, [ 1 ] ) )

    uncons ( "hello!", [] )
    -> ( "hello", Nothing )

-}
uncons : NonEmptyList a -> ( a, Maybe (NonEmptyList a) )
uncons ( h, t ) =
    ( h, fromList t )


{-| Returns first element of the nonempty list

    head ( 1, [ 2 ] )
    --> 1

-}
head : NonEmptyList a -> a
head ( h, _ ) =
    h


{-| Returns tail of the nonempty list.
The return type is List and may be empty.

    tail ( 1, [ 2, 3 ] )
    --> [2, 3]

-}
tail : NonEmptyList a -> List a
tail ( _, t ) =
    t


{-| Returns last element of the nonempty list.

    last ( 1, [ 2 ] )
    --> 2

    last ( 1, [] )
    --> 1

-}
last : NonEmptyList a -> a
last ( h, t ) =
    Maybe.withDefault h <| lastHelper t


lastHelper : List a -> Maybe a
lastHelper xs =
    case xs of
        [] ->
            Nothing

        h :: [] ->
            Just h

        _ :: t ->
            lastHelper t


{-| Removes first element of the nonempty list.

    dropHead ( 1, [ 2 ] )
    --> Just (2, [])

    dropHead ( 1, [] )
    --> Nothing

-}
dropHead : NonEmptyList a -> Maybe (NonEmptyList a)
dropHead ( _, t ) =
    fromList t



--


{-| Map a function over nonempty list.

    map (\x -> x + 1) ( 1, [ 2, 3 ] )
    --> ( 2, [3, 4] )

    map String.fromInt ( 1, [ 2 ] )
    --> ( "1", [ "2" ] )

-}
map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map f ( h, t ) =
    ( f h, List.map f t )


{-| Some as `map` but an index is passed with each element.
Index starts at 0

    indexedMap (\i x -> String.fromInt i ++ " is " ++ x) ("a", ["b", "c"])
    --> ("0 is a",["1 is b","2 is c"])

-}
indexedMap : (Int -> a -> b) -> NonEmptyList a -> NonEmptyList b
indexedMap f ( h, t ) =
    ( f 0 h, List.indexedMap (\i -> f (i + 1)) t )


foldl : (a -> b -> b) -> b -> NonEmptyList a -> b
foldl f acc =
    List.foldl f acc << toList


foldr : (a -> b -> b) -> b -> NonEmptyList a -> b
foldr f acc =
    List.foldr f acc << toList


filter : (a -> Bool) -> NonEmptyList a -> Maybe (NonEmptyList a)
filter f =
    fromList << List.filter f << toList


filterMap : (a -> Maybe b) -> NonEmptyList a -> Maybe (NonEmptyList b)
filterMap f =
    fromList << List.filterMap f << toList



--


length : NonEmptyList a -> Int
length ( _, t ) =
    List.length t + 1


reverse : NonEmptyList a -> NonEmptyList a
reverse ( h, t ) =
    case List.reverse <| h :: t of
        [] ->
            ( h, [] )

        nH :: nT ->
            ( nH, nT )


{-| Figure out whether a list contains a value.

    member 2 ( 1, [ 2 ] )
    --> True

    member 3 ( 1, [ 2 ] )
    --> False

-}
member : a -> NonEmptyList a -> Bool
member a ( h, t ) =
    a == h || List.member a t


{-| Determine if all elements satisfy some test.

    all Char.isUpper ( 'A', [ 'B' ] )
    --> True

    all Char.isUpper ( 'a', [ 'B' ] )
    --> False

-}
all : (a -> Bool) -> NonEmptyList a -> Bool
all f ( h, t ) =
    f h && List.all f t


{-| Determine if any elements satisfy test.

    any Char.isUpper ( 'a', [ 'B' ] )
    --> True

    any Char.isUpper ( 'a', [ 'b' ] )
    --> False

-}
any : (a -> Bool) -> NonEmptyList a -> Bool
any f ( h, t ) =
    f h || List.any f t


{-| Find the maximum element.

    maximum ( 3, [ 3, 5, 2 ] )
    --> 5

-}
maximum : NonEmptyList comparable -> comparable
maximum ( h, t ) =
    case List.maximum t of
        Just x ->
            if x > h then
                x

            else
                h

        Nothing ->
            h


{-| Find the minimum element.

    minimum ( 3, [ 3, 5, 2 ] )
    --> 2

-}
minimum : NonEmptyList comparable -> comparable
minimum ( h, t ) =
    case List.minimum t of
        Just x ->
            if x < h then
                x

            else
                h

        Nothing ->
            h


{-| Get the sum of the list elements.

    sum ( 2, [ 2, 2 ] )
    --> 6

-}
sum : NonEmptyList number -> number
sum =
    List.sum << toList


{-| Get the product of the list elements.

    product ( 2, [ 2, 2 ] )
    --> 8

-}
product : NonEmptyList number -> number
product =
    List.product << toList


{-| Put two lists together.

    append ( 1, [ 2, 3 ] ) ( 4, [ 5 ] )
    --> ( 1, [ 2, 3, 4, 5 ] )

-}
append : NonEmptyList a -> NonEmptyList a -> NonEmptyList a
append ne1 ne2 =
    case toList ne2 of
        [] ->
            ne1

        _ ->
            foldr cons ne2 ne1


{-| Concatenate a bunch of lists into a single list:

    concat ((1, [2, 3]), [(4, [5, 6]), (7, [8]), (9, []), (10, [11])])
    --> (1,[2,3,4,5,6,7,8,9,10,11])

-}
concat : NonEmptyList (NonEmptyList a) -> NonEmptyList a
concat ( h, t ) =
    let
        hx =
            head h

        tx =
            tail h ++ List.concat (List.map toList t)
    in
    ( hx, tx )


{-| Map a given function onto a list and flatten the resulting lists.

    concatMap singleton ( 1, [ 2 ] )
    -->  ( 1, [ 2 ] )

    concatMap (\x -> ( x + 1, [ x + 1 ] )) ( 1, [ 2 ] )
    --> ( 2, [ 2, 3, 3 ] )

-}
concatMap : (a -> NonEmptyList b) -> NonEmptyList a -> NonEmptyList b
concatMap f =
    concat << map f


{-| Create `NonEmptyList` containing sub `NoneEmptyList`s.

This is a more advanced function following [`Comonad`](https://hackage.haskell.org/package/comonad)

    duplicate ( 1, [ 2, 3 ] )
    --> ( ( 1, [ 2, 3 ] ), [ ( 2, [ 3 ] ), ( 3, [] ) ] )

    duplicate ( "alone", [] )
    --> ( ( "alone", [] ), [  ] )

-}
duplicate : NonEmptyList a -> NonEmptyList (NonEmptyList a)
duplicate ne =
    ( ne
    , case fromList <| tail ne of
        Nothing ->
            []

        Just sec ->
            List.reverse <| duplicateHelper [] sec
    )


duplicateHelper : List (NonEmptyList a) -> NonEmptyList a -> List (NonEmptyList a)
duplicateHelper acc (( _, t ) as ne) =
    case fromList t of
        Nothing ->
            ne :: acc

        Just newNE ->
            duplicateHelper (ne :: acc) newNE


{-| Take sub `NonEmptyList` for each part of `NoneEmptyList` to generate new `NonEmptyList`

This is a more advanced function following [`Comonad`](https://hackage.haskell.org/package/comonad)

    -- for each element sum all allements till the end
    extend sum ( 1, [ 2, 3 ] )
    --> ( 6, [ 5, 3 ] )

    -- calculate lenght at each point of NonEmptylist
    extend length ("foo", [ "bar", "baz", "EOF"] )
    --> ( 4, [ 3, 2, 1 ])

-}
extend : (NonEmptyList a -> b) -> NonEmptyList a -> NonEmptyList b
extend f =
    map f << duplicate


{-| Places the given value between all members of the given list.

    intersperse "and" ( "1", [ "2", "3" ] )
    --> ("1", ["and", "2", "and", "3"])

    intersperse "and" ( "1", [ "2" ] )
    --> ("1", ["and", "2"])

    intersperse "and" ( "1", [] )
    --> ("1", [])

-}
intersperse : a -> NonEmptyList a -> NonEmptyList a
intersperse x ne =
    case ne of
        ( _, [] ) ->
            ne

        ( h, t ) ->
            ( h, x :: List.intersperse x t )


{-| Combine two lists with a given function
In case where one of the two lists is longer the extra elements are ignored

    map2 (+) ( 1, [ 2 ] ) ( 1, [ 1 ] )
    --> (2, [3])

    map2 (+) ( 1, [] ) ( 1, [ 1 ] )
    --> (2, [])

    map2 (+) ( 1, [ 1 ] ) ( 1, [] )
    --> (2, [])

    map2 Tuple.pair ( 1, [ 2, 3 ]) ("foo", [ "bar" ])
    --> ( ( 1, "foo"), [ ( 2, "bar" ) ] )

-}
map2 : (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
map2 f ( h1, t1 ) ( h2, t2 ) =
    ( f h1 h2, List.map2 f t1 t2 )


{-| Map over number of nonempty lists

    map (+) (1, [2])
    |> andMap (1, [1])
    --> (2, [3])

    type alias User =
        { name : String
        , age : Int
        , admin : Bool
        }

    ( User, [ User, User ] )
    |> andMap ( "Alice", [ "Bob", "Charlie"] )
    |> andMap ( 30, [ 50, 19 ] )
    |> andMap ( True, [ False, False ])
    --> ( User "Alice" 30 True, [ User "Bob" 50 False, User "Charlie" 19 False ] )

-}
andMap : NonEmptyList a -> NonEmptyList (a -> b) -> NonEmptyList b
andMap =
    map2 (|>)


{-| unexposed sorting helper
-}
sortHelper : (List a -> List a) -> ( a, List a ) -> NonEmptyList a
sortHelper f ne =
    case f <| toList ne of
        h :: t ->
            ( h, t )

        [] ->
            -- impossible state
            ne


{-| Sort values from lowest to highest

    sort ( 3, [ 4, 1, 2 ] )
    --> (1, [2, 3, 4])

-}
sort : NonEmptyList comparable -> NonEmptyList comparable
sort =
    sortHelper List.sort


{-| Sort values by a derived property.

    sortBy String.length ( "333", [ "4444", "1", "22" ] )
    --> ("1", ["22", "333", "4444"])

-}
sortBy : (a -> comparable) -> NonEmptyList a -> NonEmptyList a
sortBy f =
    sortHelper (List.sortBy f)


{-| Sort values with a custom comparison function.
-}
sortWith : (a -> a -> Order) -> NonEmptyList a -> NonEmptyList a
sortWith f =
    sortHelper (List.sortWith f)


{-| is the nonempty list exactly one element?

    isSingleton ( 1, [] )
    --> True

    isSingleton ( 1, [ 2 ] )
    --> False

-}
isSingleton : NonEmptyList a -> Bool
isSingleton ( _, t ) =
    List.isEmpty t



{- Other function candidates:

   * take?
   * drop?
   * partition?
   * unzip?
-}
