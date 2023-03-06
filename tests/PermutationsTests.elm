module PermutationsTests exposing (foldPermutationsTest, permutationsTest)

import Expect
import Optimisation.Permutations exposing (permutations, permutations2)
import Test exposing (test, describe, Test)


permutationsTest : Test
permutationsTest =
    describe "permuations"
        [ test "One item" <|
            \_ ->
                permutations neverCurtail [ 0 ]
                    |> Expect.equal [ [ 0 ] ]
        , test "Two items" <|
            \_ ->
                permutations neverCurtail [ 0, 1 ]
                    |> Expect.equal [ [ 0, 1 ], [ 1, 0 ] ]
        , test "Three items" <|
            \_ ->
                permutations neverCurtail [ 0, 1, 2 ]
                    |> Expect.equal [ [ 0, 1, 2 ], [ 0, 2, 1 ], [ 1, 0, 2 ], [ 1, 2, 0 ], [ 2, 0, 1 ], [ 2, 1, 0 ] ]
        , test "Curtail ascending" <|
            \_ ->
                permutations curtailAscending [ 0, 1, 2 ]
                    |> Expect.equal [ [ 2, 1, 0 ] ]
        , test "Curtail consecutive number" <|
            \_ ->
                permutations curtailConsecutiveNumbers [ 0, 1, 2, 3 ]
                    |> Expect.equal [ [ 1, 3, 0, 2 ], [ 2, 0, 3, 1 ] ]
        ]


foldPermutationsTest : Test
foldPermutationsTest =
    describe "foldPermuations"
        [ test "One item" <|
            \_ ->
                permutations2 1000 neverCurtail [ 0 ]
                    |> Expect.equal [ [ 0 ] ]
        , test "Two items" <|
            \_ ->
                permutations2 1000 neverCurtail [ 0, 1 ]
                    |> Expect.equal [ [ 0, 1 ], [ 1, 0 ] ]
        , test "Three items" <|
            \_ ->
                permutations2 1000 neverCurtail [ 0, 1, 2 ]
                    |> Expect.equal [ [ 0, 1, 2 ], [ 1, 0, 2 ], [ 0, 2, 1 ], [ 2, 0, 1 ], [ 1, 2, 0 ], [ 2, 1, 0 ] ]
        , test "Curtail ascending" <|
            \_ ->
                permutations2 1000 curtailAscending [ 0, 1, 2 ]
                    |> Expect.equal [ [ 2, 1, 0 ] ]
        , test "Curtail consecutive number" <|
            \_ ->
                permutations2 1000 curtailConsecutiveNumbers [ 0, 1, 2, 3 ]
                    |> Expect.equal [ [ 1, 3, 0, 2 ], [ 2, 0, 3, 1 ] ]
        ]


curtailAscending : Int -> Int -> Bool
curtailAscending v1 v2 =
    v1 < v2


curtailConsecutiveNumbers : Int -> Int -> Bool
curtailConsecutiveNumbers v1 v2 =
    abs (v1 - v2) <= 1


neverCurtail : a -> a -> Bool
neverCurtail _ _ =
    False
