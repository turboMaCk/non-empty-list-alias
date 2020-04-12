module List.NonEmpty exposing (..)


type alias NonEmptyList a =
    ( a, List a )


singleton : a -> NonEmptyList a
singleton h =
    ( h, [] )


fromList : List a -> Maybe (NonEmptyList a)
fromList xs =
    case xs of
        h :: t ->
            Just ( h, t )

        [] ->
            Nothing


toList : NonEmptyList a -> List a
toList ( h, t ) =
    h :: t


cons : a -> NonEmptyList a -> NonEmptyList a
cons a ( h, t ) =
    ( a, h :: t )


uncons : NonEmptyList a -> ( a, Maybe (NonEmptyList a) )
uncons ( h, t ) =
    ( h, fromList t )


head : NonEmptyList a -> a
head ( h, _ ) =
    h


tail : NonEmptyList a -> List a
tail ( _, t ) =
    t


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


dropHead : NonEmptyList a -> Maybe (NonEmptyList a)
dropHead ( _, t ) =
    fromList t



--


map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map f ( h, t ) =
    ( f h, List.map f t )


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


member : a -> NonEmptyList a -> Bool
member a ( h, t ) =
    a == h || List.member a t


all : (a -> Bool) -> NonEmptyList a -> Bool
all f ( h, t ) =
    f h && List.all f t


any : (a -> Bool) -> NonEmptyList a -> Bool
any f ( h, t ) =
    f h || List.any f t


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


sum : NonEmptyList number -> number
sum =
    List.sum << toList


product : NonEmptyList number -> number
product =
    List.product << toList



-- append ("1", ["2, 3"]) ("4", ["5"])


append : NonEmptyList a -> NonEmptyList a -> NonEmptyList a
append ( h1, t1 ) ( h2, t2 ) =
    case h2 :: t2 of
        [] ->
            ( h1, t1 )

        _ ->
            foldr cons ( h2, t2 ) ( h1, t1 )



-- concat (("1", ["2", "3"]), [("4", ["5", "6"]), ("7", ["8"]), ("9", []), ("10", ["11"])])


concat : NonEmptyList (NonEmptyList a) -> NonEmptyList a
concat ( h, t ) =
    let
        hx =
            head h

        tx =
            tail h ++ List.concat (List.map toList t)
    in
    ( hx, tx )



-- concatMap (\x -> x) (("1", ["2", "3"]), [("4", ["5", "6"]), ("7", ["8"]), ("9", []), ("10", ["11"])])
-- concatMap (\x -> (x, [x+1])) (1, [2])
-- concatMap (\x -> (x+1, [x+1])) (1, [2])


concatMap : (a -> NonEmptyList b) -> NonEmptyList a -> NonEmptyList b
concatMap f l =
    concat (map f l)



-- intersperse "and" ("1", ["2", "3"])
-- intersperse "and" ("1", ["2"])
-- intersperse "and" ("1", [])


intersperse : a -> NonEmptyList a -> NonEmptyList a
intersperse x l =
    case l of
        ( _, [] ) ->
            l

        ( h, t ) ->
            ( h, x :: List.intersperse x t )



-- map2 (+) (1, [2]) (1, [1])
-- map2 (+) (1, []) (1, [1])
-- map2 (+) (1, [1]) (1, [])


map2 : (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
map2 f ( h1, t1 ) ( h2, t2 ) =
    ( f h1 h2, List.map2 f t1 t2 )



-- map (+) (1, [2]) |> andMap (1, [1])


andMap : NonEmptyList a -> NonEmptyList (a -> b) -> NonEmptyList b
andMap =
    map2 (|>)


sortHelper : (List a -> List a) -> a -> List a -> NonEmptyList a
sortHelper f h t =
    case f (h :: t) of
        h1 :: t1 ->
            ( h1, t1 )

        [] ->
            -- impossible state
            ( h, t )



-- sort (3, [4, 1, 2])


sort : NonEmptyList comparable -> NonEmptyList comparable
sort ( h, t ) =
    sortHelper List.sort h t



-- sortBy String.length ("333", ["4444", "1", "22"])


sortBy : (a -> comparable) -> NonEmptyList a -> NonEmptyList a
sortBy f ( h, t ) =
    sortHelper (List.sortBy f) h t


sortWith : (a -> a -> Order) -> NonEmptyList a -> NonEmptyList a
sortWith f ( h, t ) =
    sortHelper (List.sortWith f) h t



-- isSingleton ("1", [])
-- isSingleton ("1", ["2"])


isSingleton : NonEmptyList a -> Bool
isSingleton ( _, t ) =
    List.isEmpty t



-- take?
-- drop?
-- partition?
-- unzip?
