-- todo: expose a bit less, although tests probably still want some things to be exposed.


module Optimisation.GameOrderMetrics exposing (..)

import Array exposing (Array)
import List.Extra


type TournamentPreference
    = StartLate
    | FinishEarly
    | TwoGamesRest


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

    -- , order : Int -- don't think we should need this, try and remove it
    }


type alias AnalysedGame =
    -- use game: Game instead
    { homeTeam : Team
    , awayTeam : Team
    , homeTeamPlayingConsecutively : Bool
    , awayTeamPlayingConsecutively : Bool
    }


type alias AnalysedTeam =
    -- might be good to add team tournamentPreferenceScore here, but this type is used
    -- when calculating it, so potentially a bit annoying
    { team : Team
    , firstGame : Int
    , lastGame : Int
    , singleGameBreaks : Int
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

        analysedTeams =
            analyseTeams games
    in
    { analysedGames = analysedGames |> Array.toList
    , analysedTeams = analysedTeams
    , occurencesOfTeamsPlayingConsecutiveGames = calculateOccurencesOfTeamsPlayingConsecutiveGames analysedGames
    , tournamentPreferenceScore = calculateTournamentPreferenceScore games analysedTeams
    }


analyseGames : Array Game -> Array AnalysedGame
analyseGames games =
    Array.indexedMap
        (\index game -> analyseGame (Array.get (index - 1) games) game (Array.get (index + 1) games))
        games


analyseGame : Maybe Game -> Game -> Maybe Game -> AnalysedGame
analyseGame previousGame game nextGame =
    { homeTeam =
        game.homeTeam
    , awayTeam =
        game.awayTeam
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


analyseTeams : List Game -> List AnalysedTeam
analyseTeams games =
    let
        indexTeam index game =
            [ IndexedTeam game.homeTeam index, IndexedTeam game.awayTeam index ]

        analyseTeam : ( IndexedTeam, List IndexedTeam ) -> AnalysedTeam
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
                (AnalysedTeam indexedTeam.team indexedTeam.gameIndex indexedTeam.gameIndex 0)
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


calculateTournamentPreferenceScore : List Game -> List AnalysedTeam -> Float
calculateTournamentPreferenceScore games analysedTeams =
    List.map
        (calculateTeamTournamentPreferenceScore games)
        analysedTeams
        |> List.minimum
        |> Maybe.withDefault 0


calculateTeamTournamentPreferenceScore : List Game -> AnalysedTeam -> Float
calculateTeamTournamentPreferenceScore games analysedTeam =
    let
        teamNumberOfGames =
            List.filter (\game -> playing analysedTeam.team game) games |> List.length

        numberOfGames =
            List.length games
    in
    -- Scores should all return a value between 0 and 1
    case analysedTeam.team.tournamentPreference of
        StartLate ->
            calculateStartLateScore teamNumberOfGames numberOfGames analysedTeam

        FinishEarly ->
            calculateFinishEarlyScore teamNumberOfGames numberOfGames analysedTeam

        TwoGamesRest ->
            calculateTwoGamesRestScore teamNumberOfGames numberOfGames analysedTeam


calculateStartLateScore : Int -> Int -> AnalysedTeam -> Float
calculateStartLateScore teamNumberOfGames numberOfGames analysedTeam =
    let
        latestPossibleStart =
            numberOfGames - (teamNumberOfGames * 2 - 1)
    in
    toFloat analysedTeam.firstGame / toFloat latestPossibleStart


calculateFinishEarlyScore : Int -> Int -> AnalysedTeam -> Float
calculateFinishEarlyScore teamNumberOfGames numberOfGames analysedTeam =
    let
        earliestPossibleFinish =
            teamNumberOfGames * 2 - 1 - 1

        latestPossibleFinish =
            numberOfGames - 1
    in
    toFloat (latestPossibleFinish - analysedTeam.lastGame) / toFloat (latestPossibleFinish - earliestPossibleFinish)


calculateTwoGamesRestScore : Int -> Int -> AnalysedTeam -> Float
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
