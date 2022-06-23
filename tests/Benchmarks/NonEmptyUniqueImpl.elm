module Benchmarks.NonEmptyUniqueImpl exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import List.NonEmpty as NL exposing (NonEmpty)


uniqueFoldl : NonEmpty a -> NonEmpty a
uniqueFoldl xs =
    xs
        |> NL.foldl
            (\x acc ->
                if NL.member x acc then
                    acc

                else
                    ( NL.head acc, NL.tail acc ++ [ x ] )
            )
            (NL.singleton (NL.head xs))


suite : Benchmark
suite =
    let
        tenItems : Maybe (NonEmpty Int)
        tenItems =
            List.range 1 5
                |> List.append (List.reverse <| List.range 1 5)
                |> NL.fromList

        thousandItems : Maybe (NonEmpty Int)
        thousandItems =
            List.range 1 500
                |> List.append (List.reverse <| List.range 1 500)
                |> NL.fromList
    in
    describe "NonEmpty.unique function implementations"
        [ Benchmark.compare "10 items"
            "foldl"
            (\() -> Maybe.map uniqueFoldl tenItems)
            "uniqueHelp"
            (\() -> Maybe.map NL.unique tenItems)
        , Benchmark.compare "1000 items"
            "foldl"
            (\() -> Maybe.map uniqueFoldl thousandItems)
            "uniqueHelp"
            (\() -> Maybe.map NL.unique thousandItems)
        ]


main : BenchmarkProgram
main =
    program suite
