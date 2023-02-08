module GameOrderMetricsTests exposing (..)

import Expect
import Main exposing (..)
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
                        [ AnalysedGame (Game (team "Castle") (team "Battersea")) True False
                        , AnalysedGame (Game (team "Avon") (team "Castle")) False True
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
                        [ AnalysedGame (Game (team "Castle") (team "Avon")) True False
                        , AnalysedGame (Game (team "Battersea") (team "Castle")) False True
                        , AnalysedGame (Game (team "Avon") (team "Castle")) False True
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
                        [ AnalysedTeam (team "Castle") 0 1 1 0
                        , AnalysedTeam (team "Avon") 0 0 0 1
                        , AnalysedTeam (team "Battersea") 1 1 0 1
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
                        [ AnalysedTeam (team "Castle") 0 2 1 1 ]
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
        , test "Castle finish as early as they possibly can" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game castleFinishEarly anyTeam
                    , anyGame
                    , Game anyTeam castleFinishEarly
                    , anyGame
                    ]
                    (AnalysedTeamFirstPass castleFinishEarly 0 2 1)
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Castle finish as late as they possibly can" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game castleFinishEarly anyTeam
                    , anyGame
                    , anyGame
                    , Game anyTeam castleFinishEarly
                    ]
                    (AnalysedTeamFirstPass castleFinishEarly 0 3 1)
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 0
        , test "Battersea start as late as the possibly can" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ anyGame
                    , Game batterseaStartLate anyTeam
                    , anyGame
                    , Game anyTeam batterseaStartLate
                    ]
                    (AnalysedTeamFirstPass batterseaStartLate 1 3 1)
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Battersea start as early as the possibly can" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game batterseaStartLate anyTeam
                    , anyGame
                    , anyGame
                    , Game anyTeam batterseaStartLate
                    ]
                    (AnalysedTeamFirstPass batterseaStartLate 0 3 1)
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 0
        , test "Avon always get two games rest" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game avonTwoGamesRest anyTeam
                    , anyGame
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    ]
                    (AnalysedTeamFirstPass avonTwoGamesRest 0 3 0)
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon only play one game" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game avonTwoGamesRest anyTeam
                    , anyGame
                    , anyGame
                    ]
                    (AnalysedTeamFirstPass avonTwoGamesRest 0 0 0)
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon never get two games rest" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game avonTwoGamesRest anyTeam
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    , anyGame
                    ]
                    (AnalysedTeamFirstPass avonTwoGamesRest 0 2 1)
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 0
        , test "Avon have one single game rest" <|
            \_ ->
                analyseTeams
                    [ Game avonTwoGamesRest anyTeam
                    , anyGame
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    , anyGame
                    , anyGame
                    , anyGame
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    , anyGame
                    , anyGame
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    , anyGame
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    ]
                    |> List.filter (\analysedTeam -> analysedTeam.team == avonTwoGamesRest)
                    |> List.head
                    |> Maybe.map .singleGameBreaks
                    |> Maybe.withDefault -1
                    |> Expect.equal 1
        , test "Tournament preference score" <|
            \_ ->
                -- castle finish as early as they can, so 1
                -- avon always get two game rests, so 1
                -- battersea start as late as they can, so 1
                -- ulu only play one game, so 1
                -- clapham only play one game, so 1
                [ Game castleFinishEarly avonTwoGamesRest
                , Game batterseaStartLate claphamTwoGamesRest
                , Game uluTwoGamesRest castleFinishEarly
                , Game batterseaStartLate avonTwoGamesRest
                ]
                    |> Expect.all
                        [ \games ->
                            calculateTeamTournamentPreferenceScore games (AnalysedTeamFirstPass castleFinishEarly 0 2 1)
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games (AnalysedTeamFirstPass batterseaStartLate 1 3 1)
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games (AnalysedTeamFirstPass avonTwoGamesRest 0 3 0)
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games (AnalysedTeamFirstPass claphamTwoGamesRest 1 1 0)
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games (AnalysedTeamFirstPass uluTwoGamesRest 2 2 0)
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games (AnalysedTeamFirstPass avonTwoGamesRest 3 3 0)
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            (calculateGameOrderMetrics games).tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        ]
        ]


castleFinishEarly =
    Team "Castle" FinishEarly


avonTwoGamesRest =
    Team "Avon" TwoGamesRest


batterseaStartLate =
    Team "Battersea" StartLate


uluTwoGamesRest =
    Team "Ulu" TwoGamesRest


claphamTwoGamesRest =
    Team "Clapham" TwoGamesRest


anyTeam =
    team "Any team"


anyGame =
    Game (team "Any team 1") (team "Any team 2")


team : String -> Team
team name =
    Team name TwoGamesRest
