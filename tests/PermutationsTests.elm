module PermutationsTests exposing (permutationsTest)

-- import Main exposing (exampleGames)
-- import Array
-- import Json.Encode exposing (list)
-- import List.Extra
-- import Optimisation.GameOrderMetrics exposing (Game, Team, TournamentPreference(..), curtailWhenTeamsPlayingConsecutively, encodeGame, encodeGameOrderMetrics, initialSortOrderForGame, nullObjectGameOrderMetrics, optimiseCurtailedPermutations)

import Expect
import Optimisation.Permutations exposing (permutations)
import Test exposing (Test, describe, test)


permutationsTest : Test
permutationsTest =
    describe "permuations"
        [ test "One item" <|
            \_ ->
                permutations 1000 neverCurtail [ 0 ]
                    |> Expect.equal [ [ 0 ] ]
        , test "Two items" <|
            \_ ->
                permutations 1000 neverCurtail [ 0, 1 ]
                    |> Expect.equal [ [ 0, 1 ], [ 1, 0 ] ]
        , test "Three items" <|
            \_ ->
                permutations 1000 neverCurtail [ 0, 1, 2 ]
                    |> Expect.equal [ [ 0, 1, 2 ], [ 1, 0, 2 ], [ 0, 2, 1 ], [ 2, 0, 1 ], [ 1, 2, 0 ], [ 2, 1, 0 ] ]
        , test "Curtail ascending" <|
            \_ ->
                permutations 1000 curtailAscending [ 0, 1, 2 ]
                    |> Expect.equal [ [ 2, 1, 0 ] ]
        , test "Curtail consecutive number" <|
            \_ ->
                permutations 1000 curtailConsecutiveNumbers [ 0, 1, 2, 3 ]
                    |> Expect.equal [ [ 1, 3, 0, 2 ], [ 2, 0, 3, 1 ] ]

        -- , test "Curtail example games" <|
        --     \_ ->
        --         let
        --             sortedGames =
        --                 exampleGames
        --                     |> List.sortBy initialSortOrderForGame
        --         in
        --         permutations 500 curtailWhenTeamsPlayingConsecutively sortedGames
        --             |> Json.Encode.list (Json.Encode.list encodeGame)
        --             |> Json.Encode.encode 2
        --             |> Expect.equal ""
        -- , test "Curtail example games" <|
        --     \_ ->
        --         let
        --             sortedGames =
        --                 exampleGames
        --                     |> List.sortBy initialSortOrderForGame
        --         in
        --         optimiseCurtailedPermutations curtailWhenTeamsPlayingConsecutively sortedGames nullObjectGameOrderMetrics
        --             |> encodeGameOrderMetrics
        --             |> Json.Encode.encode 2
        --             |> Expect.equal ""
        ]



-- exampleGames : List Game
-- exampleGames =
--     [ Game (Team "ULU" EvenlySpaced) (Team "Braintree" EvenlySpaced)
--     , Game (Team "ULU" EvenlySpaced) (Team "Surrey" EvenlySpaced)
--     , Game (Team "ULU" EvenlySpaced) (Team "VKC" EvenlySpaced)
--     , Game (Team "ULU" EvenlySpaced) (Team "St Albans" FinishEarly)
--     , Game (Team "ULU" EvenlySpaced) (Team "East End" EvenlySpaced)
--     , Game (Team "ULU" EvenlySpaced) (Team "Castle" EvenlySpaced)
--     , Game (Team "Braintree" EvenlySpaced) (Team "Surrey" EvenlySpaced)
--     , Game (Team "Braintree" EvenlySpaced) (Team "VKC" EvenlySpaced)
--     , Game (Team "Braintree" EvenlySpaced) (Team "St Albans" FinishEarly)
--     , Game (Team "Braintree" EvenlySpaced) (Team "East End" EvenlySpaced)
--     , Game (Team "Braintree" EvenlySpaced) (Team "Castle" EvenlySpaced)
--     , Game (Team "Surrey" EvenlySpaced) (Team "VKC" EvenlySpaced)
--     , Game (Team "Surrey" EvenlySpaced) (Team "St Albans" FinishEarly)
--     , Game (Team "Surrey" EvenlySpaced) (Team "East End" EvenlySpaced)
--     , Game (Team "Surrey" EvenlySpaced) (Team "Castle" EvenlySpaced)
--     , Game (Team "VKC" EvenlySpaced) (Team "St Albans" FinishEarly)
--     , Game (Team "VKC" EvenlySpaced) (Team "East End" EvenlySpaced)
--     , Game (Team "VKC" EvenlySpaced) (Team "Castle" EvenlySpaced)
--     , Game (Team "St Albans" FinishEarly) (Team "East End" EvenlySpaced)
--     , Game (Team "St Albans" FinishEarly) (Team "Castle" EvenlySpaced)
--     , Game (Team "East End" EvenlySpaced) (Team "Castle" EvenlySpaced)
--     ]


curtailAscending : Int -> Int -> Bool
curtailAscending v1 v2 =
    v1 < v2



-- curtailAscendingOrEqual : Int -> Int -> Bool
-- curtailAscendingOrEqual v1 v2 =
--     v1 <= v2


curtailConsecutiveNumbers : Int -> Int -> Bool
curtailConsecutiveNumbers v1 v2 =
    abs (v1 - v2) <= 1


neverCurtail : a -> a -> Bool
neverCurtail _ _ =
    False



-- alwaysCurtail : a -> a -> Bool
-- alwaysCurtail _ _ =
--     True
