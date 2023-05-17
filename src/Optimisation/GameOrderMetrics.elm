module Optimisation.GameOrderMetrics exposing (AnalysedGame, AnalysedTeam, AnalysedTeamFirstPass, Game, GameOrderMetrics, IndexedTeam, Team, TournamentPreference(..), analyseTeams, calculateEvenlySpacedScore, calculateEvenlySpacedWithTwoGamesRestScore, calculateGameOrderMetrics, calculateTeamTournamentPreferenceScore, calculateTwoGamesRestScore, decodeGame, decodeGameOrderMetrics, decodeTeam, encodeGame, encodeGameOrderMetrics, encodeTeam, initialSortOrderForGame, optimiseAllPermutations, playing, singleGameBreaks, stringToTournamentPreference, tournamentPreferenceToString, vanillaGame, vanillaTeam)

import Array exposing (Array)
import Json.Decode
import Json.Encode
import List.Extra
import Optimisation.Permutations exposing (permutations)


type TournamentPreference
    = StartLate
    | FinishEarly
    | EvenlySpaced
    | NoPreference


encodeTournamentPreference : TournamentPreference -> Json.Encode.Value
encodeTournamentPreference tournamentPreference =
    tournamentPreferenceToString tournamentPreference
        |> Json.Encode.string


decodeTournamentPreference : Json.Decode.Decoder TournamentPreference
decodeTournamentPreference =
    -- seems like there must be a better way to do this ...
    Json.Decode.string
        |> Json.Decode.map
            stringToTournamentPreference
        |> Json.Decode.andThen
            (\m ->
                case m of
                    Just tournamentPreference ->
                        Json.Decode.succeed tournamentPreference

                    Nothing ->
                        Json.Decode.fail ""
            )


tournamentPreferenceToString : TournamentPreference -> String
tournamentPreferenceToString tournamentPreference =
    case tournamentPreference of
        StartLate ->
            "Start late"

        FinishEarly ->
            "Finish early"

        EvenlySpaced ->
            "Evenly spaced"

        NoPreference ->
            "No preference"


stringToTournamentPreference : String -> Maybe TournamentPreference
stringToTournamentPreference tournamentPreferenceString =
    case tournamentPreferenceString of
        "Start late" ->
            Just StartLate

        "Finish early" ->
            Just FinishEarly

        "Evenly spaced" ->
            Just EvenlySpaced

        "No preference" ->
            Just NoPreference

        _ ->
            Nothing


type alias Team =
    { name : String
    , tournamentPreference : TournamentPreference
    }


encodeTeam : Team -> Json.Encode.Value
encodeTeam team =
    Json.Encode.object
        [ ( "name", Json.Encode.string team.name )
        , ( "tournamentPreference", encodeTournamentPreference team.tournamentPreference )
        ]


decodeTeam : Json.Decode.Decoder Team
decodeTeam =
    Json.Decode.map2 Team
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "tournamentPreference" decodeTournamentPreference)


vanillaTeam : Team
vanillaTeam =
    Team "" EvenlySpaced


type alias GameOrderMetrics =
    { analysedGames : List AnalysedGame
    , analysedTeams : List AnalysedTeam
    , occurencesOfTeamsPlayingConsecutiveGames : Int
    , lowestTournamentPreferenceScore : Float
    , meanTournamentPreferenceScore : Float
    , highestTournamentPreferenceScore : Float
    , weightedLowestTournamentPreferenceScore : Float
    , weightedMeanTournamentPreferenceScore : Float
    }


encodeGameOrderMetrics : GameOrderMetrics -> Json.Encode.Value
encodeGameOrderMetrics gameOrderMetrics =
    Json.Encode.object
        [ ( "analysedGames", Json.Encode.list encodeAnalysedGame gameOrderMetrics.analysedGames )
        , ( "analysedTeams", Json.Encode.list encodeAnalysedTeam gameOrderMetrics.analysedTeams )
        , ( "occurencesOfTeamsPlayingConsecutiveGames", Json.Encode.int gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames )
        , ( "lowestTournamentPreferenceScore", Json.Encode.float gameOrderMetrics.lowestTournamentPreferenceScore )
        , ( "meanTournamentPreferenceScore", Json.Encode.float gameOrderMetrics.meanTournamentPreferenceScore )
        , ( "highestTournamentPreferenceScore", Json.Encode.float gameOrderMetrics.highestTournamentPreferenceScore )
        ]


decodeGameOrderMetrics : Json.Decode.Decoder (Maybe GameOrderMetrics)
decodeGameOrderMetrics =
    Json.Decode.oneOf
        [ Json.Decode.null Nothing
        , Json.Decode.map8 GameOrderMetrics
            (Json.Decode.field "analysedGames" (Json.Decode.list decodeAnalysedGame))
            (Json.Decode.field "analysedTeams" (Json.Decode.list decodeAnalysedTeam))
            (Json.Decode.field "occurencesOfTeamsPlayingConsecutiveGames" Json.Decode.int)
            (Json.Decode.field "lowestTournamentPreferenceScore" Json.Decode.float)
            (Json.Decode.field "meanTournamentPreferenceScore" Json.Decode.float)
            (Json.Decode.field "highestTournamentPreferenceScore" Json.Decode.float)
            (Json.Decode.field "weightedLowestTournamentPreferenceScore" Json.Decode.float)
            (Json.Decode.field "weightedMeanTournamentPreferenceScore" Json.Decode.float)
            |> Json.Decode.map Just
        ]


type alias Game =
    { homeTeam : Team
    , awayTeam : Team
    }


vanillaGame : Game
vanillaGame =
    Game
        vanillaTeam
        vanillaTeam


encodeGame : Game -> Json.Encode.Value
encodeGame game =
    Json.Encode.object
        [ ( "homeTeam", encodeTeam game.homeTeam )
        , ( "awayTeam", encodeTeam game.awayTeam )
        ]


decodeGame : Json.Decode.Decoder Game
decodeGame =
    Json.Decode.map2 Game
        (Json.Decode.field "homeTeam" decodeTeam)
        (Json.Decode.field "awayTeam" decodeTeam)


type alias AnalysedGame =
    { game : Game
    , homeTeamPlayingConsecutively : Bool
    , awayTeamPlayingConsecutively : Bool
    , id : Int
    }


encodeAnalysedGame : AnalysedGame -> Json.Encode.Value
encodeAnalysedGame analysedGame =
    Json.Encode.object
        [ ( "game", encodeGame analysedGame.game )
        , ( "homeTeamPlayingConsecutively", Json.Encode.bool analysedGame.homeTeamPlayingConsecutively )
        , ( "awayTeamPlayingConsecutively", Json.Encode.bool analysedGame.awayTeamPlayingConsecutively )
        , ( "id", Json.Encode.int analysedGame.id )
        ]


decodeAnalysedGame : Json.Decode.Decoder AnalysedGame
decodeAnalysedGame =
    Json.Decode.map4 AnalysedGame
        (Json.Decode.field "game" decodeGame)
        (Json.Decode.field "homeTeamPlayingConsecutively" Json.Decode.bool)
        (Json.Decode.field "awayTeamPlayingConsecutively" Json.Decode.bool)
        (Json.Decode.field "id" Json.Decode.int)


type alias AnalysedTeamFirstPass =
    { team : Team
    , firstGame : Int
    , lastGame : Int
    , gameBreaks : List Int
    }


type alias AnalysedTeam =
    { team : Team
    , firstGame : Int
    , lastGame : Int
    , gameBreaks : List Int
    , tournamentPreferenceScore : Float
    }


encodeAnalysedTeam : AnalysedTeam -> Json.Encode.Value
encodeAnalysedTeam analysedTeam =
    Json.Encode.object
        [ ( "team", encodeTeam analysedTeam.team )
        , ( "firstGame", Json.Encode.int analysedTeam.firstGame )
        , ( "lastGame", Json.Encode.int analysedTeam.lastGame )
        , ( "gameBreaks", Json.Encode.list Json.Encode.int analysedTeam.gameBreaks )
        , ( "tournamentPreferenceScore", Json.Encode.float analysedTeam.tournamentPreferenceScore )
        ]


decodeAnalysedTeam : Json.Decode.Decoder AnalysedTeam
decodeAnalysedTeam =
    Json.Decode.map5 AnalysedTeam
        (Json.Decode.field "team" decodeTeam)
        (Json.Decode.field "firstGame" Json.Decode.int)
        (Json.Decode.field "lastGame" Json.Decode.int)
        (Json.Decode.field "gameBreaks" (Json.Decode.list Json.Decode.int))
        (Json.Decode.field "tournamentPreferenceScore" Json.Decode.float)


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
    if curtailedGameOrderMetrics == initialGameOrderMetrics then
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
            permutations 1000000 curtail games
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
    , weightedLowestTournamentPreferenceScore = 0
    , weightedMeanTournamentPreferenceScore = 0
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
        gameOrderMetrics1.weightedLowestTournamentPreferenceScore
            > gameOrderMetrics2.weightedLowestTournamentPreferenceScore
    then
        gameOrderMetrics1

    else if
        gameOrderMetrics2.weightedLowestTournamentPreferenceScore
            > gameOrderMetrics1.weightedLowestTournamentPreferenceScore
    then
        gameOrderMetrics2

    else if
        gameOrderMetrics1.weightedMeanTournamentPreferenceScore
            > gameOrderMetrics2.weightedMeanTournamentPreferenceScore
    then
        gameOrderMetrics1

    else if
        gameOrderMetrics2.weightedMeanTournamentPreferenceScore
            > gameOrderMetrics1.weightedMeanTournamentPreferenceScore
    then
        gameOrderMetrics2

    else if
        gameOrderMetrics1.highestTournamentPreferenceScore
            > gameOrderMetrics2.highestTournamentPreferenceScore
    then
        gameOrderMetrics1

    else
        gameOrderMetrics2


tournamentPreferenceMeanWeight : TournamentPreference -> Float
tournamentPreferenceMeanWeight tournamentPreference =
    -- This makes very little difference, as game orders are initially
    -- compared on the minimum
    case tournamentPreference of
        StartLate ->
            0.5

        FinishEarly ->
            0.5

        EvenlySpaced ->
            1

        NoPreference ->
            1


tournamentPreferenceMinWeight : TournamentPreference -> Float
tournamentPreferenceMinWeight tournamentPreference =
    case tournamentPreference of
        StartLate ->
            0.5

        FinishEarly ->
            0.5

        EvenlySpaced ->
            1

        NoPreference ->
            1



-- order the games in a very roughly optimised order, hopefully so that we need to
-- analyse less permutations before finding a good result


initialSortOrderForGame : Game -> Int
initialSortOrderForGame game =
    if game.homeTeam.tournamentPreference == FinishEarly || game.awayTeam.tournamentPreference == FinishEarly then
        -1

    else if game.homeTeam.tournamentPreference == StartLate || game.awayTeam.tournamentPreference == StartLate then
        1

    else
        0


calculateGameOrderMetrics : List Game -> GameOrderMetrics
calculateGameOrderMetrics games =
    let
        analysedGames : Array AnalysedGame
        analysedGames =
            games
                |> Array.fromList
                |> analyseGames

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

        weightedLowestTournamentPreferenceScore : Float
        weightedLowestTournamentPreferenceScore =
            List.map (\analysedTeam -> analysedTeam.tournamentPreferenceScore * tournamentPreferenceMinWeight analysedTeam.team.tournamentPreference) analysedTeams |> List.minimum |> Maybe.withDefault 0

        meanTournamentPreferenceScore : Float
        meanTournamentPreferenceScore =
            (List.map .tournamentPreferenceScore analysedTeams |> List.sum)
                / toFloat (List.length analysedTeams)

        weightedMeanTournamentPreferenceScore : Float
        weightedMeanTournamentPreferenceScore =
            (List.map (\analysedTeam -> analysedTeam.tournamentPreferenceScore * tournamentPreferenceMeanWeight analysedTeam.team.tournamentPreference) analysedTeams
                |> List.sum
            )
                / (List.map (\analysedTeam -> tournamentPreferenceMeanWeight analysedTeam.team.tournamentPreference) analysedTeams
                    |> List.sum
                  )

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
    , weightedLowestTournamentPreferenceScore = weightedLowestTournamentPreferenceScore
    , weightedMeanTournamentPreferenceScore = weightedMeanTournamentPreferenceScore
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
                , gameBreaks = (indexedTeam2.gameIndex - analysedTeam.lastGame) :: analysedTeam.gameBreaks
            }
        )
        (AnalysedTeamFirstPass indexedTeam.team indexedTeam.gameIndex indexedTeam.gameIndex [])
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

                    EvenlySpaced ->
                        calculateEvenlySpacedWithTwoGamesRestScore (calculateTeamNumberOfGames games analysedTeamFirstPass) (List.length games) analysedTeamFirstPass

                    NoPreference ->
                        1
    in
    { team = analysedTeamFirstPass.team
    , firstGame = analysedTeamFirstPass.firstGame
    , lastGame = analysedTeamFirstPass.lastGame
    , gameBreaks = analysedTeamFirstPass.gameBreaks
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


calculateEvenlySpacedWithTwoGamesRestScore : Int -> Int -> AnalysedTeamFirstPass -> Float
calculateEvenlySpacedWithTwoGamesRestScore teamNumberOfGames numberOfGames analysedTeam =
    min
        (calculateEvenlySpacedScore teamNumberOfGames numberOfGames analysedTeam)
        (calculateTwoGamesRestScore teamNumberOfGames numberOfGames analysedTeam)


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
            - toFloat (singleGameBreaks analysedTeam.gameBreaks - minimumPossibleSingleGameBreaks)
            / toFloat (teamNumberOfGames - 1 - minimumPossibleSingleGameBreaks)


calculateEvenlySpacedScore : Int -> Int -> AnalysedTeamFirstPass -> Float
calculateEvenlySpacedScore teamNumberOfGames numberOfGames analysedTeam =
    let
        earliestPossiblePenultimateGame : Int
        earliestPossiblePenultimateGame =
            (teamNumberOfGames - 1) * 2 - 1

        worstFeasibleSpacing : Int
        worstFeasibleSpacing =
            numberOfGames - earliestPossiblePenultimateGame - 2
    in
    if worstFeasibleSpacing == 0 then
        1

    else
        let
            spacing : Int
            spacing =
                (List.maximum analysedTeam.gameBreaks |> Maybe.withDefault 0) - (List.minimum analysedTeam.gameBreaks |> Maybe.withDefault 0)
        in
        toFloat (worstFeasibleSpacing - spacing) / toFloat worstFeasibleSpacing


singleGameBreaks : List Int -> Int
singleGameBreaks gameBreaks =
    List.Extra.count ((==) 2) gameBreaks



-- calculateTwoGamesRestCompactionScore : Int -> Int -> AnalysedTeamFirstPass -> Float
-- calculateTwoGamesRestCompactionScore teamNumberOfGames numberOfGames analysedTeam =
--     -- delete / comment this if we don't use it
--     let
--         worstPossibleCompaction : Int
--         worstPossibleCompaction =
--             numberOfGames - 1
--         bestPossibleCompaction : Int
--         bestPossibleCompaction =
--             teamNumberOfGames * 3 - 2 - 1
--     in
--     if worstPossibleCompaction - bestPossibleCompaction <= 0 then
--         1
--     else
--         let
--             teamCompaction : Int
--             teamCompaction =
--                 analysedTeam.lastGame - analysedTeam.firstGame
--         in
--         1 - (toFloat (teamCompaction - bestPossibleCompaction) / toFloat (worstPossibleCompaction - bestPossibleCompaction))
