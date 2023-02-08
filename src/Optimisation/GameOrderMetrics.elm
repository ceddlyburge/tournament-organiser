-- todo: expose a bit less, although tests probably still want some things to be exposed.


module Optimisation.GameOrderMetrics exposing (..)

import Array exposing (Array)
import List.Extra


type TournamentPreference
    = StartLate
    | FinishEarly
    | TwoGamesRest
    | NoPreference


type alias Team =
    { name : String
    , tournamentPreference : TournamentPreference
    }


vanillaTeam : Team
vanillaTeam =
    Team "" TwoGamesRest


type alias GameOrderMetrics =
    { analysedGames : List AnalysedGame
    , analysedTeams : List AnalysedTeam
    , occurencesOfTeamsPlayingConsecutiveGames : Int
    , tournamentPreferenceScore : Float
    }


type alias Game =
    { homeTeam : Team
    , awayTeam : Team
    }


type alias AnalysedGame =
    { game : Game
    , homeTeamPlayingConsecutively : Bool
    , awayTeamPlayingConsecutively : Bool
    }


type alias AnalysedTeamFirstPass =
    { team : Team
    , firstGame : Int
    , lastGame : Int
    , singleGameBreaks : Int
    }


type alias AnalysedTeam =
    { team : Team
    , firstGame : Int
    , lastGame : Int
    , singleGameBreaks : Int
    , tournamentPreferenceScore : Float
    }


type alias IndexedTeam =
    { team : Team
    , gameIndex : Int
    }


calculateGameOrderMetrics : List Game -> GameOrderMetrics
calculateGameOrderMetrics games =
    let
        analysedGames =
            analyseGames (Array.fromList games)

        analysedTeamsFirstPass =
            analyseTeams games

        analysedTeams =
            calculateTournamentPreferenceScores games analysedTeamsFirstPass

        tournamentPreferenceScore =
            List.map .tournamentPreferenceScore analysedTeams |> List.minimum |> Maybe.withDefault 0
    in
    { analysedGames = analysedGames |> Array.toList
    , analysedTeams = analysedTeams
    , occurencesOfTeamsPlayingConsecutiveGames = calculateOccurencesOfTeamsPlayingConsecutiveGames analysedGames
    , tournamentPreferenceScore = tournamentPreferenceScore
    }


analyseGames : Array Game -> Array AnalysedGame
analyseGames games =
    Array.indexedMap
        (\index game -> analyseGame (Array.get (index - 1) games) game (Array.get (index + 1) games))
        games


analyseGame : Maybe Game -> Game -> Maybe Game -> AnalysedGame
analyseGame previousGame game nextGame =
    { game = game
    , homeTeamPlayingConsecutively = maybePlaying game.homeTeam previousGame || maybePlaying game.homeTeam nextGame
    , awayTeamPlayingConsecutively = maybePlaying game.awayTeam previousGame || maybePlaying game.awayTeam nextGame
    }


playing : Team -> Game -> Bool
playing team game =
    game.homeTeam == team || game.awayTeam == team


curtailWhenTeamsPlayingConsecutively : Game -> Game -> Bool
curtailWhenTeamsPlayingConsecutively game1 game2 =
    playing game1.homeTeam game2 || playing game1.awayTeam game2


maybePlaying : Team -> Maybe Game -> Bool
maybePlaying team maybeGame =
    Maybe.map
        (\game -> game.homeTeam == team || game.awayTeam == team)
        maybeGame
        |> Maybe.withDefault False


analyseTeams : List Game -> List AnalysedTeamFirstPass
analyseTeams games =
    let
        indexTeam index game =
            [ IndexedTeam game.homeTeam index, IndexedTeam game.awayTeam index ]

        analyseTeam : ( IndexedTeam, List IndexedTeam ) -> AnalysedTeamFirstPass
        analyseTeam ( indexedTeam, indexedTeams ) =
            List.foldr
                (\indexedTeam2 analysedTeam ->
                    { analysedTeam
                        | firstGame = min analysedTeam.firstGame indexedTeam2.gameIndex
                        , lastGame = max analysedTeam.lastGame indexedTeam2.gameIndex
                        , singleGameBreaks =
                            analysedTeam.singleGameBreaks
                                + (if indexedTeam2.gameIndex - analysedTeam.lastGame <= 2 then
                                    1

                                   else
                                    0
                                  )
                    }
                )
                (AnalysedTeamFirstPass indexedTeam.team indexedTeam.gameIndex indexedTeam.gameIndex 0)
                indexedTeams
    in
    List.indexedMap indexTeam games
        |> List.concat
        |> List.Extra.gatherWith (\team1 team2 -> team1.team == team2.team)
        |> List.map analyseTeam


calculateOccurencesOfTeamsPlayingConsecutiveGames : Array AnalysedGame -> Int
calculateOccurencesOfTeamsPlayingConsecutiveGames analysedGames =
    Array.foldr
        (\analysedGame count ->
            count
                + (if analysedGame.homeTeamPlayingConsecutively then
                    1

                   else
                    0
                  )
                + (if analysedGame.awayTeamPlayingConsecutively then
                    1

                   else
                    0
                  )
        )
        0
        analysedGames
        // 2


calculateTournamentPreferenceScores : List Game -> List AnalysedTeamFirstPass -> List AnalysedTeam
calculateTournamentPreferenceScores games analysedTeams =
    List.map
        (calculateTeamTournamentPreferenceScore games)
        analysedTeams


calculateTeamTournamentPreferenceScore : List Game -> AnalysedTeamFirstPass -> AnalysedTeam
calculateTeamTournamentPreferenceScore games analysedTeamFirstPass =
    let
        teamNumberOfGames =
            List.filter (\game -> playing analysedTeamFirstPass.team game) games |> List.length

        numberOfGames =
            List.length games

        score =
            case analysedTeamFirstPass.team.tournamentPreference of
                StartLate ->
                    calculateStartLateScore teamNumberOfGames numberOfGames analysedTeamFirstPass

                FinishEarly ->
                    calculateFinishEarlyScore teamNumberOfGames numberOfGames analysedTeamFirstPass

                TwoGamesRest ->
                    calculateTwoGamesRestScore teamNumberOfGames numberOfGames analysedTeamFirstPass

                NoPreference ->
                    1

        analysedTeam =
            { team = analysedTeamFirstPass.team
            , firstGame = analysedTeamFirstPass.firstGame
            , lastGame = analysedTeamFirstPass.lastGame
            , singleGameBreaks = analysedTeamFirstPass.singleGameBreaks
            , tournamentPreferenceScore = score
            }
    in
    -- Scores should all return a value between 0 and 1
    if score < 0 || score > 1 then
        let
            _ =
                Debug.log "score" score

            _ =
                Debug.log "games" games

            _ =
                Debug.log "analysedTeams" analysedTeam

            _ =
                Debug.log "teamNumberOfGames" teamNumberOfGames
        in
        analysedTeam

    else
        analysedTeam


calculateStartLateScore : Int -> Int -> AnalysedTeamFirstPass -> Float
calculateStartLateScore teamNumberOfGames numberOfGames analysedTeam =
    let
        latestPossibleStart =
            numberOfGames - (teamNumberOfGames * 2 - 1)
    in
    toFloat analysedTeam.firstGame / toFloat latestPossibleStart


calculateFinishEarlyScore : Int -> Int -> AnalysedTeamFirstPass -> Float
calculateFinishEarlyScore teamNumberOfGames numberOfGames analysedTeam =
    let
        earliestPossibleFinish =
            teamNumberOfGames * 2 - 1 - 1

        latestPossibleFinish =
            numberOfGames - 1
    in
    toFloat (latestPossibleFinish - analysedTeam.lastGame) / toFloat (latestPossibleFinish - earliestPossibleFinish)


calculateTwoGamesRestScore : Int -> Int -> AnalysedTeamFirstPass -> Float
calculateTwoGamesRestScore teamNumberOfGames numberOfGames analysedTeam =
    let
        numberOfGamesToAlwaysAllowTwoGameRests =
            teamNumberOfGames * 3 - 2

        minimumPossibleSingleGameBreaks =
            max (numberOfGamesToAlwaysAllowTwoGameRests - numberOfGames) 0
    in
    if teamNumberOfGames - 1 - minimumPossibleSingleGameBreaks == 0 then
        1

    else
        1
            - toFloat (analysedTeam.singleGameBreaks - minimumPossibleSingleGameBreaks)
            / toFloat (teamNumberOfGames - 1 - minimumPossibleSingleGameBreaks)
