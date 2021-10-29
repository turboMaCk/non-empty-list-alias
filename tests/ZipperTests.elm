module ZipperTests exposing (..)

import Expect
import List.NonEmpty.Zipper as Zipper exposing (Zipper)
import Test exposing (..)


simpleZipper : Zipper Int
simpleZipper =
    List.range 1 9
        |> Tuple.pair 0
        |> Zipper.fromNonEmpty


queryingTest : Test
queryingTest =
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
        , test "attemptPrev << attemptNext is identity" <|
            \() ->
                Zipper.attemptNext simpleZipper
                    |> Zipper.attemptPrev
                    |> Expect.equal simpleZipper
        , test "prev on first is Nothing" <|
            \() ->
                Zipper.prev simpleZipper
                    |> Expect.equal Nothing
        , test "nextBy number lower than size" <|
            \() ->
                Zipper.nextBy 5 simpleZipper
                    |> Maybe.map Zipper.current
                    |> Expect.equal (Just 5)
        , test "nextBy more than length returns Nothing" <|
            \() ->
                Zipper.nextBy 10 simpleZipper
                    |> Expect.equal Nothing
        , test "prevBy on first should return Nothing" <|
            \() ->
                Zipper.prevBy 3 simpleZipper
                    |> Expect.equal Nothing
        , test "prevBy 1 =<< nextBy 1 should return original" <|
            \() ->
                Zipper.nextBy 1 simpleZipper
                    |> Maybe.andThen (Zipper.prevBy 1)
                    |> Expect.equal (Just simpleZipper)
        , test "attemptNextBy moves by n" <|
            \() ->
                Zipper.attemptNextBy 4 simpleZipper
                    |> Zipper.current
                    |> Expect.equal 4
        , test "attemptNextBy stays on last element if greater than length" <|
            \() ->
                Zipper.attemptNextBy 9999 simpleZipper
                    |> Zipper.current
                    |> Expect.equal 9
        , test "attemptPrevBy moves back" <|
            \() ->
                Zipper.attemptNextBy 5 simpleZipper
                    |> Zipper.attemptPrevBy 3
                    |> Zipper.current
                    |> Expect.equal 2
        ]


cycleTest : Test
cycleTest =
    describe "cycling movement"
        [ test "forward cycles back to first" <|
            \() ->
                Zipper.forward simpleZipper
                    |> Zipper.forward
                    |> Zipper.forward
                    |> Zipper.forward
                    |> Zipper.forward
                    |> Zipper.forward
                    |> Zipper.forward
                    |> Zipper.forward
                    |> Zipper.forward
                    |> Zipper.forward
                    |> Expect.equal simpleZipper
        , test "backward leads to last" <|
            \() ->
                Zipper.backward simpleZipper
                    |> Zipper.current
                    |> Expect.equal 9
        , test "forwardBy length on first returns to beginning" <|
            \() ->
                Zipper.forwardBy (Zipper.length simpleZipper) simpleZipper
                    |> Expect.equal simpleZipper
        , test "forwardBy should make n steps" <|
            \() ->
                Zipper.forwardBy 3 simpleZipper
                    |> Zipper.current
                    |> Expect.equal 3
        , test "backwardBy should make n steps" <|
            \() ->
                Zipper.backwardBy 3 simpleZipper
                    |> Zipper.current
                    |> Expect.equal 7
        ]


comonadTest : Test
comonadTest =
    describe "comonad"
        [ test "extend current is identity" <|
            \() ->
                Zipper.extend Zipper.current simpleZipper
                    |> Expect.equal simpleZipper
        , test "extend identity is duplicate" <|
            \() ->
                Zipper.extend identity simpleZipper
                    |> Expect.equal (Zipper.duplicate simpleZipper)
        ]


applicativeTest : Test
applicativeTest =
    describe "applicative"
        [ test "uniformly distributed" <|
            \() ->
                Zipper.map (+) simpleZipper
                    |> Zipper.andMap simpleZipper
                    |> Expect.equal (Zipper.map ((*) 2) simpleZipper)
        ]


toEnds : Test
toEnds =
    describe "rewind to ends" <|
        [ test "rewind to start" <|
            \() ->
                Zipper.backward simpleZipper
                    |> Zipper.start
                    |> Expect.equal simpleZipper
        , test "rewind to end" <|
            \() ->
                Zipper.end simpleZipper
                    |> Expect.equal (Zipper.backward simpleZipper)
        , test "rewind to end is same as attemptNext by length - 1" <|
            \() ->
                Zipper.attemptNextBy (Zipper.length simpleZipper - 1) simpleZipper
                    |> Expect.equal (Zipper.end simpleZipper)
        ]
