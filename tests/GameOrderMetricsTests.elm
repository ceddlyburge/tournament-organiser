module GameOrderMetricsTests exposing (all)

import Expect
import List
import Optimisation.GameOrderMetrics exposing (AnalysedGame, AnalysedTeam, AnalysedTeamFirstPass, Game, Team, TournamentPreference(..), analyseTeams, calculateEvenlySpacedScore, calculateEvenlySpacedWithTwoGamesRestScore, calculateGameOrderMetrics, calculateTeamTournamentPreferenceScore, calculateTwoGamesRestScore, optimiseAllPermutations, singleGameBreaks)
import Test exposing (Test, describe, test)


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
                        [ AnalysedGame (Game (team "Castle") (team "Battersea")) True False 0
                        , AnalysedGame (Game (team "Avon") (team "Castle")) False True 1
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
                        [ AnalysedGame (Game (team "Castle") (team "Avon")) True False 0
                        , AnalysedGame (Game (team "Battersea") (team "Castle")) False True 1
                        , AnalysedGame (Game (team "Avon") (team "Castle")) False True 2
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
                        [ AnalysedTeam (team "Castle") 0 1 [ 1 ] 0
                        , AnalysedTeam (team "Avon") 0 0 [] 0
                        , AnalysedTeam (team "Battersea") 1 1 [] 0
                        ]
        , test "Single game breaks" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Avon")
                    , Game (team "Battersea") (team "Blackwater")
                    , Game (team "Meridian") (team "Castle")
                    ]
                    |> .analysedTeams
                    |> List.filter (\aTeam -> singleGameBreaks aTeam.gameBreaks > 0)
                    |> Expect.equal
                        [ AnalysedTeam (team "Castle") 0 2 [ 2 ] 1 ]
        , test "No single game breaks" <|
            \_ ->
                calculateGameOrderMetrics
                    [ Game (team "Castle") (team "Avon")
                    , Game (team "Battersea") (team "Blackwater")
                    , Game (team "Ulu") (team "Clapham")
                    , Game (team "Meridian") (team "Castle")
                    ]
                    |> .analysedTeams
                    |> List.filter (\aTeam -> singleGameBreaks aTeam.gameBreaks > 0)
                    |> Expect.equal []
        , test "Castle finish as early as they possibly can" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game castleFinishEarly anyTeam
                    , anyGame
                    , Game anyTeam castleFinishEarly
                    , anyGame
                    ]
                    0
                    (AnalysedTeamFirstPass castleFinishEarly 0 2 [ 2 ])
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
                    0
                    (AnalysedTeamFirstPass castleFinishEarly 0 3 [ 3 ])
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
                    0
                    (AnalysedTeamFirstPass batterseaStartLate 1 3 [ 2 ])
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
                    0
                    (AnalysedTeamFirstPass batterseaStartLate 0 3 [ 2 ])
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 0
        , test "Avon always get two games rest" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- ]
                calculateTwoGamesRestScore 2 4 (AnalysedTeamFirstPass avonTwoGamesRest 1 4 [ 3 ])
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon only play one game, so always get two games rest" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , anyGame
                -- ]
                calculateTwoGamesRestScore 1 3 (AnalysedTeamFirstPass avonTwoGamesRest 1 1 [])
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon never get two games rest" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- , anyGame
                -- ]
                calculateTwoGamesRestScore 2 4 (AnalysedTeamFirstPass avonTwoGamesRest 1 3 [ 2 ])
                    |> Expect.within (Expect.Absolute 0.0001) 0
        , test "Avon perfectly compact with two games rest" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- ]
                calculateEvenlySpacedWithTwoGamesRestScore 2 4 (AnalysedTeamFirstPass avonTwoGamesRest 1 4 [ 3 ])
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon only play one game, so perfectly compact" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , anyGame
                -- ]
                calculateEvenlySpacedWithTwoGamesRestScore 1 3 (AnalysedTeamFirstPass avonTwoGamesRest 1 1 [])
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon better than perfect compaction" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- , anyGame
                -- ]
                calculateEvenlySpacedWithTwoGamesRestScore 2 4 (AnalysedTeamFirstPass avonTwoGamesRest 1 3 [ 2 ])
                    |> Expect.within (Expect.Absolute 0.0001) 0
        , test "Avon evenly spaced" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- ]
                calculateEvenlySpacedScore 2 4 (AnalysedTeamFirstPass avonTwoGamesRest 1 4 [ 3 ])
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon only play one game, so evenly spaced" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , anyGame
                -- ]
                calculateEvenlySpacedScore 1 3 (AnalysedTeamFirstPass avonTwoGamesRest 1 1 [])
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon evenly spaced by a single game" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- , anyGame
                -- ]
                calculateEvenlySpacedScore 2 4 (AnalysedTeamFirstPass avonTwoGamesRest 1 3 [])
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon spaced unevenly" <|
            \_ ->
                -- [ Game avonTwoGamesRest anyTeam
                -- , anyGame
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- , anyGame
                -- , anyGame
                -- , anyGame
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- , anyGame
                -- , anyGame
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- , anyGame
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- , anyGame
                -- , Game anyTeam avonTwoGamesRest
                -- ]
                calculateEvenlySpacedScore 6 18 (AnalysedTeamFirstPass avonTwoGamesRest 1 18 [ 3, 5, 4, 3, 2 ])
                    |> Expect.within (Expect.Absolute 0.0001) (toFloat (7 - 3) / 7)
        , test "Avon always get two games rest, perfectly compact" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game avonTwoGamesRest anyTeam
                    , anyGame
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    ]
                    0
                    (AnalysedTeamFirstPass avonTwoGamesRest 0 3 [ 3 ])
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon only play one game" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game avonTwoGamesRest anyTeam
                    , anyGame
                    , anyGame
                    ]
                    0
                    (AnalysedTeamFirstPass avonTwoGamesRest 0 0 [])
                    |> .tournamentPreferenceScore
                    |> Expect.within (Expect.Absolute 0.0001) 1
        , test "Avon never get two games rest, but good compaction" <|
            \_ ->
                calculateTeamTournamentPreferenceScore
                    [ Game avonTwoGamesRest anyTeam
                    , anyGame
                    , Game anyTeam avonTwoGamesRest
                    , anyGame
                    ]
                    0
                    (AnalysedTeamFirstPass avonTwoGamesRest 0 2 [ 2 ])
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
                    |> Maybe.map (\aTeam -> singleGameBreaks aTeam.gameBreaks)
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
                            calculateTeamTournamentPreferenceScore games 0 (AnalysedTeamFirstPass castleFinishEarly 0 2 [ 2 ])
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games 0 (AnalysedTeamFirstPass batterseaStartLate 1 3 [ 2 ])
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games 0 (AnalysedTeamFirstPass avonTwoGamesRest 0 3 [ 3 ])
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games 0 (AnalysedTeamFirstPass claphamTwoGamesRest 1 1 [])
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            calculateTeamTournamentPreferenceScore games 0 (AnalysedTeamFirstPass uluTwoGamesRest 2 2 [])
                                |> .tournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        , \games ->
                            (calculateGameOrderMetrics games).lowestTournamentPreferenceScore
                                |> Expect.within (Expect.Absolute 0.0001) 1
                        ]
        , test "Repond with something when permutations get out of hand and curtailment isn't possible" <|
            \_ ->
                optimiseAllPermutations
                    [ Game castleFinishEarly avonTwoGamesRest
                    , Game batterseaStartLate claphamTwoGamesRest
                    , Game claphamTwoGamesRest castleFinishEarly
                    , Game batterseaStartLate avonTwoGamesRest
                    , Game castleFinishEarly avonTwoGamesRest
                    , Game batterseaStartLate claphamTwoGamesRest
                    , Game claphamTwoGamesRest castleFinishEarly
                    , Game batterseaStartLate avonTwoGamesRest
                    , Game castleFinishEarly avonTwoGamesRest
                    , Game batterseaStartLate claphamTwoGamesRest
                    , Game claphamTwoGamesRest castleFinishEarly
                    , Game batterseaStartLate avonTwoGamesRest
                    , Game castleFinishEarly avonTwoGamesRest
                    , Game batterseaStartLate claphamTwoGamesRest
                    , Game claphamTwoGamesRest castleFinishEarly
                    , Game batterseaStartLate avonTwoGamesRest
                    , Game castleFinishEarly avonTwoGamesRest
                    , Game batterseaStartLate claphamTwoGamesRest
                    , Game claphamTwoGamesRest castleFinishEarly
                    , Game batterseaStartLate avonTwoGamesRest
                    ]
                    Nothing
                    |> .analysedGames
                    |> List.length
                    |> Expect.equal 20
        ]


castleFinishEarly : Team
castleFinishEarly =
    Team "Castle" FinishEarly


avonTwoGamesRest : Team
avonTwoGamesRest =
    Team "Avon" EvenlySpaced


batterseaStartLate : Team
batterseaStartLate =
    Team "Battersea" StartLate


uluTwoGamesRest : Team
uluTwoGamesRest =
    Team "Ulu" EvenlySpaced


claphamTwoGamesRest : Team
claphamTwoGamesRest =
    Team "Clapham" EvenlySpaced


anyTeam : Team
anyTeam =
    team "Any team"


anyGame : Game
anyGame =
    Game (team "Any team 1") (team "Any team 2")


team : String -> Team
team name =
    Team name EvenlySpaced
