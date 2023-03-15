-- todo: expose a bit less, although tests probably still want some things to be exposed.


module Optimisation.GameOrderMetrics exposing (AnalysedGame, AnalysedTeam, AnalysedTeamFirstPass, Game, GameOrderMetrics, IndexedTeam, Team, TournamentPreference(..), analyseTeams, calculateGameOrderMetrics, calculateTeamTournamentPreferenceScore, optimiseAllPermutations, playing, tournamentPreferenceFromString, tournamentPreferenceToString, vanillaGame, vanillaTeam)

import Array exposing (Array)
import List.Extra
import Optimisation.Permutations exposing (permutations2)


type TournamentPreference
    = StartLate
    | FinishEarly
    | TwoGamesRest
    | NoPreference


tournamentPreferenceToString : TournamentPreference -> String
tournamentPreferenceToString tournamentPreference =
    case tournamentPreference of
        StartLate ->
            "Start late"

        FinishEarly ->
            "Finish early"

        TwoGamesRest ->
            "Two games rest"

        NoPreference ->
            "No preference"


tournamentPreferenceFromString : String -> TournamentPreference
tournamentPreferenceFromString tournamentPreferenceString =
    case tournamentPreferenceString of
        "Start late" ->
            StartLate

        "Finish early" ->
            FinishEarly

        "Two games rest" ->
            TwoGamesRest

        _ ->
            NoPreference


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
    , lowestTournamentPreferenceScore : Float
    , meanTournamentPreferenceScore : Float
    , highestTournamentPreferenceScore : Float
    }


type alias Game =
    { homeTeam : Team
    , awayTeam : Team
    }


vanillaGame : Game
vanillaGame =
    Game
        vanillaTeam
        vanillaTeam


type alias AnalysedGame =
    { game : Game
    , homeTeamPlayingConsecutively : Bool
    , awayTeamPlayingConsecutively : Bool
    , id : Int
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


optimiseAllPermutations : List Game -> Maybe GameOrderMetrics -> GameOrderMetrics
optimiseAllPermutations games maybeGameOrderMetrics =
    let
        initialGameOrderMetrics : GameOrderMetrics
        initialGameOrderMetrics =
            maybeGameOrderMetrics
                |> Maybe.withDefault nullObjectGameOrderMetrics

        curtailedGameOrderMetrics : GameOrderMetrics
        curtailedGameOrderMetrics =
            optimiseCurtailedPermutations curtailWhenTeamsPlayingConsecutively games initialGameOrderMetrics
    in
    if curtailedGameOrderMetrics == nullObjectGameOrderMetrics then
        optimiseCurtailedPermutations neverCurtail games initialGameOrderMetrics

    else
        curtailedGameOrderMetrics


optimiseCurtailedPermutations : (Game -> Game -> Bool) -> List Game -> GameOrderMetrics -> GameOrderMetrics
optimiseCurtailedPermutations curtail games gameOrderMetrics =
    let
        -- todo, pass calculateGameOrderMetrics to the permuter, so don't have
        -- to store all the gameOrders, and can instead analyse them as we
        -- find them. Less memory usage. And probably a little bit faster.
        gameOrders : List (List Game)
        gameOrders =
            permutations2 100000 curtail games
    in
    List.foldr
        (\gameOrder currentBestGameOrderMetrics ->
            bestGameOrderMetrics (calculateGameOrderMetrics gameOrder) currentBestGameOrderMetrics
        )
        gameOrderMetrics
        gameOrders


nullObjectGameOrderMetrics : GameOrderMetrics
nullObjectGameOrderMetrics =
    { analysedGames = []
    , analysedTeams = []
    , occurencesOfTeamsPlayingConsecutiveGames = 2 ^ 31 - 1
    , lowestTournamentPreferenceScore = 0
    , meanTournamentPreferenceScore = 0
    , highestTournamentPreferenceScore = 0
    }


bestGameOrderMetrics : GameOrderMetrics -> GameOrderMetrics -> GameOrderMetrics
bestGameOrderMetrics gameOrderMetrics1 gameOrderMetrics2 =
    --If the minimum of the metrics is equal, then the next thing to differentiate on is the average. If the average is also the same, then go for the one with the lowest max (which means that teams will be treated as fairly as possible)
    if
        gameOrderMetrics1.occurencesOfTeamsPlayingConsecutiveGames
            < gameOrderMetrics2.occurencesOfTeamsPlayingConsecutiveGames
    then
        gameOrderMetrics1

    else if
        gameOrderMetrics2.occurencesOfTeamsPlayingConsecutiveGames
            < gameOrderMetrics1.occurencesOfTeamsPlayingConsecutiveGames
    then
        gameOrderMetrics2

    else if
        gameOrderMetrics1.lowestTournamentPreferenceScore
            > gameOrderMetrics2.lowestTournamentPreferenceScore
    then
        gameOrderMetrics1

    else if
        gameOrderMetrics2.lowestTournamentPreferenceScore
            > gameOrderMetrics1.lowestTournamentPreferenceScore
    then
        gameOrderMetrics2

    else if
        gameOrderMetrics1.meanTournamentPreferenceScore
            > gameOrderMetrics2.meanTournamentPreferenceScore
    then
        gameOrderMetrics1

    else if
        gameOrderMetrics2.meanTournamentPreferenceScore
            > gameOrderMetrics1.meanTournamentPreferenceScore
    then
        gameOrderMetrics2

    else if
        gameOrderMetrics1.highestTournamentPreferenceScore
            > gameOrderMetrics2.highestTournamentPreferenceScore
    then
        gameOrderMetrics1

    else
        gameOrderMetrics2


calculateGameOrderMetrics : List Game -> GameOrderMetrics
calculateGameOrderMetrics games =
    let
        analysedGames : Array AnalysedGame
        analysedGames =
            analyseGames (Array.fromList games)

        occurencesOfTeamsPlayingConsecutiveGames : Int
        occurencesOfTeamsPlayingConsecutiveGames =
            calculateOccurencesOfTeamsPlayingConsecutiveGames analysedGames

        analysedTeamsFirstPass : List AnalysedTeamFirstPass
        analysedTeamsFirstPass =
            analyseTeams games

        analysedTeams : List AnalysedTeam
        analysedTeams =
            calculateTournamentPreferenceScores games analysedTeamsFirstPass occurencesOfTeamsPlayingConsecutiveGames

        lowestTournamentPreferenceScore : Float
        lowestTournamentPreferenceScore =
            List.map .tournamentPreferenceScore analysedTeams |> List.minimum |> Maybe.withDefault 0

        meanTournamentPreferenceScore : Float
        meanTournamentPreferenceScore =
            List.map .tournamentPreferenceScore analysedTeams |> List.sum |> (\sum -> sum / toFloat (List.length analysedTeams))

        highestTournamentPreferenceScore : Float
        highestTournamentPreferenceScore =
            List.map .tournamentPreferenceScore analysedTeams |> List.maximum |> Maybe.withDefault 0
    in
    { analysedGames = analysedGames |> Array.toList
    , analysedTeams = analysedTeams
    , occurencesOfTeamsPlayingConsecutiveGames = occurencesOfTeamsPlayingConsecutiveGames
    , lowestTournamentPreferenceScore = lowestTournamentPreferenceScore
    , meanTournamentPreferenceScore = meanTournamentPreferenceScore
    , highestTournamentPreferenceScore = highestTournamentPreferenceScore
    }


analyseGames : Array Game -> Array AnalysedGame
analyseGames games =
    Array.indexedMap
        (\index game -> analyseGame (Array.get (index - 1) games) game (Array.get (index + 1) games) index)
        games


analyseGame : Maybe Game -> Game -> Maybe Game -> Int -> AnalysedGame
analyseGame previousGame game nextGame index =
    { game = game
    , homeTeamPlayingConsecutively = maybePlaying game.homeTeam previousGame || maybePlaying game.homeTeam nextGame
    , awayTeamPlayingConsecutively = maybePlaying game.awayTeam previousGame || maybePlaying game.awayTeam nextGame
    , id = index
    }


playing : Team -> Game -> Bool
playing team game =
    game.homeTeam.name == team.name || game.awayTeam.name == team.name


curtailWhenTeamsPlayingConsecutively : Game -> Game -> Bool
curtailWhenTeamsPlayingConsecutively game1 game2 =
    playing game1.homeTeam game2 || playing game1.awayTeam game2


neverCurtail : Game -> Game -> Bool
neverCurtail _ _ =
    False


maybePlaying : Team -> Maybe Game -> Bool
maybePlaying team maybeGame =
    Maybe.map
        (\game -> game.homeTeam == team || game.awayTeam == team)
        maybeGame
        |> Maybe.withDefault False


analyseTeams : List Game -> List AnalysedTeamFirstPass
analyseTeams games =
    let
        indexTeam : Int -> { a | homeTeam : Team, awayTeam : Team } -> List IndexedTeam
        indexTeam index game =
            [ IndexedTeam game.homeTeam index, IndexedTeam game.awayTeam index ]
    in
    List.indexedMap indexTeam games
        |> List.concat
        |> List.Extra.gatherWith (\team1 team2 -> team1.team == team2.team)
        |> List.map analyseTeam


analyseTeam : ( IndexedTeam, List IndexedTeam ) -> AnalysedTeamFirstPass
analyseTeam ( indexedTeam, indexedTeams ) =
    List.foldl
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


calculateTournamentPreferenceScores : List Game -> List AnalysedTeamFirstPass -> Int -> List AnalysedTeam
calculateTournamentPreferenceScores games analysedTeams occurencesOfTeamsPlayingConsecutiveGames =
    List.map
        (calculateTeamTournamentPreferenceScore games occurencesOfTeamsPlayingConsecutiveGames)
        analysedTeams


calculateTeamNumberOfGames : List Game -> AnalysedTeamFirstPass -> Int
calculateTeamNumberOfGames games analysedTeamFirstPass =
    List.filter (\game -> playing analysedTeamFirstPass.team game) games |> List.length


calculateTeamTournamentPreferenceScore : List Game -> Int -> AnalysedTeamFirstPass -> AnalysedTeam
calculateTeamTournamentPreferenceScore games occurencesOfTeamsPlayingConsecutiveGames analysedTeamFirstPass =
    let
        score : Float
        score =
            if occurencesOfTeamsPlayingConsecutiveGames > 0 then
                0

            else
                case analysedTeamFirstPass.team.tournamentPreference of
                    StartLate ->
                        calculateStartLateScore (calculateTeamNumberOfGames games analysedTeamFirstPass) (List.length games) analysedTeamFirstPass

                    FinishEarly ->
                        calculateFinishEarlyScore (calculateTeamNumberOfGames games analysedTeamFirstPass) (List.length games) analysedTeamFirstPass

                    TwoGamesRest ->
                        calculateTwoGamesRestScore (calculateTeamNumberOfGames games analysedTeamFirstPass) (List.length games) analysedTeamFirstPass

                    NoPreference ->
                        1
    in
    { team = analysedTeamFirstPass.team
    , firstGame = analysedTeamFirstPass.firstGame
    , lastGame = analysedTeamFirstPass.lastGame
    , singleGameBreaks = analysedTeamFirstPass.singleGameBreaks
    , tournamentPreferenceScore = score
    }


calculateStartLateScore : Int -> Int -> AnalysedTeamFirstPass -> Float
calculateStartLateScore teamNumberOfGames numberOfGames analysedTeam =
    let
        latestPossibleStart : Int
        latestPossibleStart =
            numberOfGames - (teamNumberOfGames * 2 - 1)
    in
    toFloat analysedTeam.firstGame / toFloat latestPossibleStart


calculateFinishEarlyScore : Int -> Int -> AnalysedTeamFirstPass -> Float
calculateFinishEarlyScore teamNumberOfGames numberOfGames analysedTeam =
    let
        earliestPossibleFinish : Int
        earliestPossibleFinish =
            teamNumberOfGames * 2 - 1 - 1

        latestPossibleFinish : Int
        latestPossibleFinish =
            numberOfGames - 1
    in
    toFloat (latestPossibleFinish - analysedTeam.lastGame) / toFloat (latestPossibleFinish - earliestPossibleFinish)


calculateTwoGamesRestScore : Int -> Int -> AnalysedTeamFirstPass -> Float
calculateTwoGamesRestScore teamNumberOfGames numberOfGames analysedTeam =
    let
        numberOfGamesToAlwaysAllowTwoGameRests : Int
        numberOfGamesToAlwaysAllowTwoGameRests =
            teamNumberOfGames * 3 - 2

        minimumPossibleSingleGameBreaks : Int
        minimumPossibleSingleGameBreaks =
            max (numberOfGamesToAlwaysAllowTwoGameRests - numberOfGames) 0
    in
    if teamNumberOfGames - 1 - minimumPossibleSingleGameBreaks == 0 then
        1

    else
        1
            - toFloat (analysedTeam.singleGameBreaks - minimumPossibleSingleGameBreaks)
            / toFloat (teamNumberOfGames - 1 - minimumPossibleSingleGameBreaks)
