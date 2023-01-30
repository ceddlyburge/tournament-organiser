module GameOrderMetricsTests exposing (..)

import Expect
import Main exposing (Team, TournamentPreference(..))
import Optimisation.GameOrderMetrics exposing (..)
import Test exposing (..)


all : Test
all =
    describe "GameOrderMetrics"
        [ test "First And Last Teams Should Be Marked As Playing Consecutively" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Battersea")
                    , Game (team "Avon") (team "Castle")
                    ]
                    |> .analysedGames
                    |> Expect.equal
                        [ AnalysedGame (team "Castle") (team "Battersea") True False
                        , AnalysedGame (team "Avon") (team "Castle") False True
                        ]
        , test "Middle Teams Should Be Marked As Playing Consecutively" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Avon")
                    , Game (team "Battersea") (team "Castle")
                    , Game (team "Avon") (team "Castle")
                    ]
                    |> .analysedGames
                    |> Expect.equal
                        [ AnalysedGame (team "Castle") (team "Avon") True False
                        , AnalysedGame (team "Battersea") (team "Castle") False True
                        , AnalysedGame (team "Avon") (team "Castle") False True
                        ]
        , test "One team playing consecutively once" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Avon")
                    , Game (team "Battersea") (team "Castle")
                    ]
                    |> .occurencesOfTeamsPlayingConsecutiveGames
                    |> Expect.equal 1
        , test "Two teams playing consecutively once" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Avon")
                    , Game (team "Avon") (team "Castle")
                    ]
                    |> .occurencesOfTeamsPlayingConsecutiveGames
                    |> Expect.equal 2
        , test "Analysed teams" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Avon")
                    , Game (team "Battersea") (team "Castle")
                    ]
                    |> .analysedTeams
                    |> Expect.equal
                        [ AnalysedTeam (team "Castle") 0 1 1
                        , AnalysedTeam (team "Avon") 0 0 0
                        , AnalysedTeam (team "Battersea") 1 1 0
                        ]
        , test "Single game breaks" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Avon")
                    , Game (team "Battersea") (team "Blackwater")
                    , Game (team "Meridian") (team "Castle")
                    ]
                    |> .analysedTeams
                    |> List.filter (\aTeam -> aTeam.singleGameBreaks > 0)
                    |> Expect.equal
                        [ AnalysedTeam (team "Castle") 0 2 1 ]
        , test "No single game breaks" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Avon")
                    , Game (team "Battersea") (team "Blackwater")
                    , Game (team "Ulu") (team "Clapham")
                    , Game (team "Meridian") (team "Castle")
                    ]
                    |> .analysedTeams
                    |> List.filter (\aTeam -> aTeam.singleGameBreaks > 0)
                    |> Expect.equal []
        ]


team : String -> Team
team name =
    Team name TwoGamesRest
