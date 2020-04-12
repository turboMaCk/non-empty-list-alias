module ZipperTests exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import Test exposing (..)


simpleZipper : Zipper Int
simpleZipper =
    List.range 1 9
        |> Tuple.pair 0
        |> Zipper.fromNonEmpty


queringTest : Test
queringTest =
    describe "querying"
        [ test "get current at first" <|
            \() ->
                Zipper.current simpleZipper
                    |> Expect.equal 0
        , test "listPrev at first returns empty list" <|
            \() ->
                Zipper.listPrev simpleZipper
                    |> Expect.equal []
        , test "listNext at first returns list of remaining" <|
            \() ->
                Zipper.listNext simpleZipper
                    |> Expect.equal (List.range 1 9)
        , test "listPrev returns items in correct order" <|
            \() ->
                Zipper.attemptNext simpleZipper
                    |> Zipper.attemptNext
                    |> Zipper.attemptNext
                    |> Zipper.listPrev
                    |> Expect.equal [ 0, 1, 2 ]
        ]


movementTest : Test
movementTest =
    describe "movement of focus"
        [ test "next is moving focus" <|
            \() ->
                Zipper.next simpleZipper
                    |> Maybe.map Zipper.current
                    |> Expect.equal (Just 1)
        , test "attemptNext << attemptPrev is identity" <|
            \() ->
                Zipper.attemptNext simpleZipper
                    |> Zipper.attemptPrev
                    |> Expect.equal simpleZipper
        ]
