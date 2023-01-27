module Permutations2Tests exposing (..)

import Array
import Expect
import Optimisation.Permutations2 exposing (..)
import Test exposing (..)


iteration1 =
    Array.fromList [ Element 0 left, Element 1 left, Element 2 left, Element 3 left ]


iteration2 =
    Array.fromList [ Element 0 left, Element 1 left, Element 3 left, Element 2 left ]


iteration3 =
    Array.fromList [ Element 0 left, Element 3 left, Element 1 left, Element 2 left ]


iteration4 =
    Array.fromList [ Element 3 left, Element 0 left, Element 1 left, Element 2 left ]


iteration5 =
    Array.fromList [ Element 3 right, Element 0 left, Element 2 left, Element 1 left ]


iteration6 =
    Array.fromList [ Element 0 left, Element 3 right, Element 2 left, Element 1 left ]


iteration7 =
    Array.fromList [ Element 0 left, Element 2 left, Element 3 right, Element 1 left ]


iteration8 =
    Array.fromList [ Element 0 left, Element 2 left, Element 1 left, Element 3 right ]


iteration9 =
    Array.fromList [ Element 2 left, Element 0 left, Element 1 left, Element 3 left ]


iteration10 =
    Array.fromList [ Element 2 left, Element 0 left, Element 3 left, Element 1 left ]


iteration11 =
    Array.fromList [ Element 2 left, Element 3 left, Element 0 left, Element 1 left ]


iteration12 =
    Array.fromList [ Element 3 left, Element 2 left, Element 0 left, Element 1 left ]


iteration13 =
    Array.fromList [ Element 3 right, Element 2 right, Element 1 left, Element 0 left ]


iteration14 =
    Array.fromList [ Element 2 right, Element 3 right, Element 1 left, Element 0 left ]


iteration15 =
    Array.fromList [ Element 2 right, Element 1 left, Element 3 right, Element 0 left ]


iteration16 =
    Array.fromList [ Element 2 right, Element 1 left, Element 0 left, Element 3 right ]


iteration17 =
    Array.fromList [ Element 1 left, Element 2 right, Element 0 left, Element 3 left ]


iteration18 =
    Array.fromList [ Element 1 left, Element 2 right, Element 3 left, Element 0 left ]


iteration19 =
    Array.fromList [ Element 1 left, Element 3 left, Element 2 right, Element 0 left ]


iteration20 =
    Array.fromList [ Element 3 left, Element 1 left, Element 2 right, Element 0 left ]


iteration21 =
    Array.fromList [ Element 3 right, Element 1 left, Element 0 left, Element 2 right ]


iteration22 =
    Array.fromList [ Element 1 left, Element 3 right, Element 0 left, Element 2 right ]


iteration23 =
    Array.fromList [ Element 1 left, Element 0 left, Element 3 right, Element 2 right ]


iteration24 =
    Array.fromList [ Element 1 left, Element 0 left, Element 2 right, Element 3 right ]


for : Int -> (a -> a) -> a -> a
for =
    let
        for_ : Int -> Int -> (a -> a) -> a -> a
        for_ i n f v =
            if i < n then
                for_ (i + 1) n f (f v)

            else
                v
    in
    for_ 0


testNext : Test
testNext =
    describe "Next"
        [ test "Iteration 1" <|
            \_ ->
                first [ "a", "b", "c" ]
                    |> getState
                    |> Maybe.map next
                    |> Maybe.andThen getPermutation
                    |> Expect.equal (Just [ "a", "c", "b" ])
        , test "Iteration 2" <|
            \_ ->
                Just (first [ "a", "b", "c" ])
                    |> for 2 (Maybe.andThen getState >> Maybe.map next)
                    |> Maybe.andThen getPermutation
                    |> Expect.equal (Just [ "c", "a", "b" ])
        , test "Iteration 3" <|
            \_ ->
                Just (first [ "a", "b", "c" ])
                    |> for 3 (Maybe.andThen getState >> Maybe.map next)
                    |> Maybe.andThen getPermutation
                    |> Expect.equal (Just [ "c", "b", "a" ])
        , test "Iteration 4" <|
            \_ ->
                Just (first [ "a", "b", "c" ])
                    |> for 4 (Maybe.andThen getState >> Maybe.map next)
                    |> Maybe.andThen getPermutation
                    |> Expect.equal (Just [ "b", "c", "a" ])
        , test "Iteration 5" <|
            \_ ->
                Just (first [ "a", "b", "c" ])
                    |> for 5 (Maybe.andThen getState >> Maybe.map next)
                    |> Maybe.andThen getPermutation
                    |> Expect.equal (Just [ "b", "a", "c" ])
        , test "Iteration 6" <|
            \_ ->
                Just (first [ "a", "b", "c" ])
                    |> for 6 (Maybe.andThen getState >> Maybe.map next)
                    |> Expect.equal (Just Done)
        ]


testNextPermutation : Test
testNextPermutation =
    describe "Next Permutation"
        [ test "Iteration 1" <|
            \_ ->
                calculateNextPermutation iteration1
                    |> Expect.equal (Just iteration2)
        , test "Iteration 2" <|
            \_ ->
                calculateNextPermutation iteration2
                    |> Expect.equal (Just iteration3)
        , test "Iteration 3" <|
            \_ ->
                calculateNextPermutation iteration3
                    |> Expect.equal (Just iteration4)
        , test "Iteration 4" <|
            \_ ->
                calculateNextPermutation iteration4
                    |> Expect.equal (Just iteration5)
        , test "Iteration 5" <|
            \_ ->
                calculateNextPermutation iteration5
                    |> Expect.equal (Just iteration6)
        , test "Iteration 6" <|
            \_ ->
                calculateNextPermutation iteration6
                    |> Expect.equal (Just iteration7)
        , test "Iteration 7" <|
            \_ ->
                calculateNextPermutation iteration7
                    |> Expect.equal (Just iteration8)
        , test "Iteration 8" <|
            \_ ->
                calculateNextPermutation iteration8
                    |> Expect.equal (Just iteration9)
        , test "Iteration 9" <|
            \_ ->
                calculateNextPermutation iteration9
                    |> Expect.equal (Just iteration10)
        , test "Iteration 10" <|
            \_ ->
                calculateNextPermutation iteration10
                    |> Expect.equal (Just iteration11)
        , test "Iteration 11" <|
            \_ ->
                calculateNextPermutation iteration11
                    |> Expect.equal (Just iteration12)
        , test "Iteration 12" <|
            \_ ->
                calculateNextPermutation iteration12
                    |> Expect.equal (Just iteration13)
        , test "Iteration 13" <|
            \_ ->
                calculateNextPermutation iteration13
                    |> Expect.equal (Just iteration14)
        , test "Iteration 14" <|
            \_ ->
                calculateNextPermutation iteration14
                    |> Expect.equal (Just iteration15)
        , test "Iteration 15" <|
            \_ ->
                calculateNextPermutation iteration15
                    |> Expect.equal (Just iteration16)
        , test "Iteration 16" <|
            \_ ->
                calculateNextPermutation iteration16
                    |> Expect.equal (Just iteration17)
        , test "Iteration 17" <|
            \_ ->
                calculateNextPermutation iteration17
                    |> Expect.equal (Just iteration18)
        , test "Iteration 18" <|
            \_ ->
                calculateNextPermutation iteration18
                    |> Expect.equal (Just iteration19)
        , test "Iteration 19" <|
            \_ ->
                calculateNextPermutation iteration19
                    |> Expect.equal (Just iteration20)
        , test "Iteration 20" <|
            \_ ->
                calculateNextPermutation iteration20
                    |> Expect.equal (Just iteration21)
        , test "Iteration 21" <|
            \_ ->
                calculateNextPermutation iteration21
                    |> Expect.equal (Just iteration22)
        , test "Iteration 22" <|
            \_ ->
                calculateNextPermutation iteration22
                    |> Expect.equal (Just iteration23)
        , test "Iteration 23" <|
            \_ ->
                calculateNextPermutation iteration23
                    |> Expect.equal (Just iteration24)
        , test "Iteration 24" <|
            \_ ->
                calculateNextPermutation iteration24
                    |> Expect.equal Nothing
        ]


testGetMobile : Test
testGetMobile =
    describe "Mobile"
        [ test "Iteration 1" <|
            \_ ->
                calculateMobile iteration1
                    |> Expect.equal (Just ( 3, Element 3 left ))

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
