module Permutations2Tests exposing (..)

import Array
import Expect
import Optimisation.Permutations2 exposing (..)
import Test exposing (..)


iteration1 =
    Array.fromList [ Element 1 left, Element 2 left, Element 3 left, Element 4 left ]


iteration2 =
    Array.fromList [ Element 1 left, Element 2 left, Element 4 left, Element 3 left ]


iteration3 =
    Array.fromList [ Element 1 left, Element 4 left, Element 2 left, Element 3 left ]


iteration4 =
    Array.fromList [ Element 4 left, Element 1 left, Element 2 left, Element 3 left ]


iteration5 =
    Array.fromList [ Element 4 right, Element 1 left, Element 3 left, Element 2 left ]


iteration6 =
    Array.fromList [ Element 1 left, Element 4 right, Element 3 left, Element 2 left ]


iteration7 =
    Array.fromList [ Element 1 left, Element 3 left, Element 4 right, Element 2 left ]


iteration8 =
    Array.fromList [ Element 1 left, Element 3 left, Element 2 left, Element 4 right ]


iteration9 =
    Array.fromList [ Element 3 left, Element 1 left, Element 2 left, Element 4 left ]


iteration10 =
    Array.fromList [ Element 3 left, Element 1 left, Element 4 left, Element 2 left ]


iteration11 =
    Array.fromList [ Element 3 left, Element 4 left, Element 1 left, Element 2 left ]


iteration12 =
    Array.fromList [ Element 4 left, Element 3 left, Element 1 left, Element 2 left ]


iteration13 =
    Array.fromList [ Element 4 right, Element 3 right, Element 2 left, Element 1 left ]


iteration14 =
    Array.fromList [ Element 3 right, Element 4 right, Element 2 left, Element 1 left ]


iteration15 =
    Array.fromList [ Element 3 right, Element 2 left, Element 4 right, Element 1 left ]


iteration16 =
    Array.fromList [ Element 3 right, Element 2 left, Element 1 left, Element 4 right ]


iteration17 =
    Array.fromList [ Element 2 left, Element 3 right, Element 1 left, Element 4 left ]


iteration18 =
    Array.fromList [ Element 2 left, Element 3 right, Element 4 left, Element 1 left ]


iteration19 =
    Array.fromList [ Element 2 left, Element 4 left, Element 3 right, Element 1 left ]


iteration20 =
    Array.fromList [ Element 4 left, Element 2 left, Element 3 right, Element 1 left ]


iteration21 =
    Array.fromList [ Element 4 right, Element 2 left, Element 1 left, Element 3 right ]


iteration22 =
    Array.fromList [ Element 2 left, Element 4 right, Element 1 left, Element 3 right ]


iteration23 =
    Array.fromList [ Element 2 left, Element 1 left, Element 4 right, Element 3 right ]


iteration24 =
    Array.fromList [ Element 2 left, Element 1 left, Element 3 right, Element 4 right ]


testNextPermutation : Test
testNextPermutation =
    describe "Next Permutation"
        [ test "Iteration 1" <|
            \_ ->
                nextPermutation (State iteration1 (Array.fromList [ "a", "b", "c", "d" ]))
                    |> Expect.equal (Next (State iteration2 (Array.fromList [ "a", "b", "c", "d" ])) [ "a", "b", "d", "c" ])

        -- , test "Iteration 2" <|
        --     \_ ->
        --         nextPermutation iteration2
        --             |> Expect.equal iteration3
        -- , test "Iteration 3" <|
        --     \_ ->
        --         nextPermutation iteration3
        --             |> Expect.equal iteration4
        -- , test "Iteration 4" <|
        --     \_ ->
        --         nextPermutation iteration4
        --             |> Expect.equal iteration5
        -- , test "Iteration 5" <|
        --     \_ ->
        --         nextPermutation iteration5
        --             |> Expect.equal iteration6
        -- , test "Iteration 6" <|
        --     \_ ->
        --         nextPermutation iteration6
        --             |> Expect.equal iteration7
        -- , test "Iteration 7" <|
        --     \_ ->
        --         nextPermutation iteration7
        --             |> Expect.equal iteration8
        -- , test "Iteration 8" <|
        --     \_ ->
        --         nextPermutation iteration8
        --             |> Expect.equal iteration9
        -- , test "Iteration 9" <|
        --     \_ ->
        --         nextPermutation iteration9
        --             |> Expect.equal iteration10
        -- , test "Iteration 10" <|
        --     \_ ->
        --         nextPermutation iteration10
        --             |> Expect.equal iteration11
        -- , test "Iteration 11" <|
        --     \_ ->
        --         nextPermutation iteration11
        --             |> Expect.equal iteration12
        -- , test "Iteration 12" <|
        --     \_ ->
        --         nextPermutation iteration12
        --             |> Expect.equal iteration13
        -- , test "Iteration 13" <|
        --     \_ ->
        --         nextPermutation iteration13
        --             |> Expect.equal iteration14
        -- , test "Iteration 14" <|
        --     \_ ->
        --         nextPermutation iteration14
        --             |> Expect.equal iteration15
        -- , test "Iteration 15" <|
        --     \_ ->
        --         nextPermutation iteration15
        --             |> Expect.equal iteration16
        -- , test "Iteration 16" <|
        --     \_ ->
        --         nextPermutation iteration16
        --             |> Expect.equal iteration17
        -- , test "Iteration 17" <|
        --     \_ ->
        --         nextPermutation iteration17
        --             |> Expect.equal iteration18
        -- , test "Iteration 18" <|
        --     \_ ->
        --         nextPermutation iteration18
        --             |> Expect.equal iteration19
        -- , test "Iteration 19" <|
        --     \_ ->
        --         nextPermutation iteration19
        --             |> Expect.equal iteration20
        -- , test "Iteration 20" <|
        --     \_ ->
        --         nextPermutation iteration20
        --             |> Expect.equal iteration21
        -- , test "Iteration 21" <|
        --     \_ ->
        --         nextPermutation iteration21
        --             |> Expect.equal iteration22
        -- , test "Iteration 22" <|
        --     \_ ->
        --         nextPermutation iteration22
        --             |> Expect.equal iteration23
        -- , test "Iteration 23" <|
        --     \_ ->
        --         nextPermutation iteration23
        --             |> Expect.equal iteration24
        -- , test "Iteration 24" <|
        --     \_ ->
        --         nextPermutation iteration24
        --             |> Expect.equal iteration24
        ]


testGetMobile : Test
testGetMobile =
    describe "Mobile"
        [ test "Iteration 1" <|
            \_ ->
                calculateMobile iteration1
                    |> Expect.equal (Just ( 3, Element 4 left ))

        -- , test "Iteration 2" <|
        --     \_ ->
        --         getMobile iteration2
        --             |> Expect.equal 2
        -- , test "Iteration 3" <|
        --     \_ ->
        --         getMobile iteration3
        --             |> Expect.equal 1
        -- , test "Iteration 4" <|
        --     \_ ->
        --         getMobile iteration4
        --             |> Expect.equal 3
        -- , test "Iteration 5" <|
        --     \_ ->
        --         getMobile iteration5
        --             |> Expect.equal 0
        -- , test "Iteration 6" <|
        --     \_ ->
        --         getMobile iteration6
        --             |> Expect.equal 1
        -- , test "Iteration 7" <|
        --     \_ ->
        --         getMobile iteration7
        --             |> Expect.equal 2
        -- , test "Iteration 8" <|
        --     \_ ->
        --         getMobile iteration8
        --             |> Expect.equal 1
        -- , test "Iteration 9" <|
        --     \_ ->
        --         getMobile iteration9
        --             |> Expect.equal 3
        -- , test "Iteration 10" <|
        --     \_ ->
        --         getMobile iteration10
        --             |> Expect.equal 2
        -- , test "Iteration 11" <|
        --     \_ ->
        --         getMobile iteration11
        --             |> Expect.equal 1
        -- , test "Iteration 12" <|
        --     \_ ->
        --         getMobile iteration12
        --             |> Expect.equal 3
        -- , test "Iteration 13" <|
        --     \_ ->
        --         getMobile iteration13
        --             |> Expect.equal 0
        -- , test "Iteration 14" <|
        --     \_ ->
        --         getMobile iteration14
        --             |> Expect.equal 1
        -- , test "Iteration 15" <|
        --     \_ ->
        --         getMobile iteration15
        --             |> Expect.equal 2
        -- , test "Iteration 16" <|
        --     \_ ->
        --         getMobile iteration16
        --             |> Expect.equal 0
        -- , test "Iteration 17" <|
        --     \_ ->
        --         getMobile iteration17
        --             |> Expect.equal 3
        -- , test "Iteration 18" <|
        --     \_ ->
        --         getMobile iteration18
        --             |> Expect.equal 2
        -- , test "Iteration 19" <|
        --     \_ ->
        --         getMobile iteration19
        --             |> Expect.equal 1
        -- , test "Iteration 20" <|
        --     \_ ->
        --         getMobile iteration20
        --             |> Expect.equal 2
        -- , test "Iteration 21" <|
        --     \_ ->
        --         getMobile iteration21
        --             |> Expect.equal 0
        -- , test "Iteration 22" <|
        --     \_ ->
        --         getMobile iteration22
        --             |> Expect.equal 1
        -- , test "Iteration 23" <|
        --     \_ ->
        --         getMobile iteration23
        --             |> Expect.equal 2
        -- , test "Iteration 24" <|
        --     \_ ->
        --         getMobile iteration24
        --             |> Expect.equal -1
        ]



-- describe "Permuations"
--     [ test "One item" <|
--         \_ ->
--             permutations [ 0 ]
--                 |> Expect.equal [ [ 0 ] ]
--     , test "Two items" <|
--         \_ ->
--             permutations [ 0, 1 ]
--                 |> Expect.equal [ [ 0, 1 ], [ 1, 0 ] ]
--     , test "Three items" <|
--         \_ ->
--             permutations [ 0, 1, 2 ]
--                 |> Expect.equal [ [ 0, 1, 2 ], [ 0, 2, 1 ], [ 1, 0, 2 ], [ 1, 2, 0 ], [ 2, 0, 1 ], [ 2, 1, 0 ] ]
--     ]
