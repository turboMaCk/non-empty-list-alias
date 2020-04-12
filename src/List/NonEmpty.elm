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



--
-- append
-- concat
-- concatMap
-- intersperse
-- map2
-- andMap
--
-- sort
-- sortBy
-- sortWith
-- isSingleton
-- take?
-- drop?
-- partition?
-- unzip?
