module Main exposing (..)

-- import Json.Decode
-- import Json.Decode.Pipeline
-- import Json.Encode as Encode
-- import Http
-- import RemoteData exposing (WebData)

import Browser
import Html exposing (Html, a, button, div, h1, h2, header, img, input, li, main_, nav, option, p, section, select, span, text, ul)
import Html.Attributes exposing (alt, class, src, value, width)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Optimisation.GameOrderMetrics exposing (AnalysedGame, Game, GameOrderMetrics, Team, TournamentPreference(..), calculateGameOrderMetrics, curtailWhenTeamsPlayingConsecutively, vanillaTeam)
import Optimisation.Permutations exposing (permutations)
import Optimisation.Permutations2 exposing (Step(..), first, next)
import Svg exposing (Svg)
import Svg.Attributes exposing (in_)



---- MODEL ----
-- type to represent the result of the optimization api
-- type alias Optimisation =
--     { optimisedGameOrder : OptimisedGameOrder
--     , pragmatisationLevel : Int
--     , optimisationMessage : String
--     , optimisedGameOrderAvailable : Bool
--     }
-- type alias OptimisedGameOrder =
--     { gameOrder : List OptimisedGame
--     , occurencesOfTeamsPlayingConsecutiveGames : Int
--     , maxPlayingInConsecutiveGames : Int
--     , gamesNotPlayedBetweenFirstAndLast : Int
--     }
-- type alias OptimisedTeam =
--     { name : String
--     }
-- type alias OptimisedGame =
--     { homeTeamPlayingConsecutively : Bool
--     , awayTeamPlayingConsecutively : Bool
--     , homeTeam : OptimisedTeam
--     , awayTeam : OptimisedTeam
--     }
-- decodeOptimisation : Json.Decode.Decoder Optimisation
-- decodeOptimisation =
--     Json.Decode.succeed Optimisation
--         |> Json.Decode.Pipeline.required "optimisedGameOrder" decodeOptimisedGames
--         |> Json.Decode.Pipeline.required "pragmatisationLevel" Json.Decode.int
--         |> Json.Decode.Pipeline.required "optimisationMessage" Json.Decode.string
--         |> Json.Decode.Pipeline.required "optimisedGameOrderAvailable" Json.Decode.bool
-- decodeOptimisedGames : Json.Decode.Decoder OptimisedGameOrder
-- decodeOptimisedGames =
--     Json.Decode.succeed OptimisedGameOrder
--         |> Json.Decode.Pipeline.required "gameOrder" (Json.Decode.list decodeOptimisedGame)
--         |> Json.Decode.Pipeline.required "occurencesOfTeamsPlayingConsecutiveGames" Json.Decode.int
--         |> Json.Decode.Pipeline.required "maxPlayingInConsecutiveGames" Json.Decode.int
--         |> Json.Decode.Pipeline.required "gamesNotPlayedBetweenFirstAndLast" Json.Decode.int
-- decodeOptimisedGame : Json.Decode.Decoder OptimisedGame
-- decodeOptimisedGame =
--     Json.Decode.succeed OptimisedGame
--         |> Json.Decode.Pipeline.required "homeTeamPlayingConsecutively" Json.Decode.bool
--         |> Json.Decode.Pipeline.required "awayTeamPlayingConsecutively" Json.Decode.bool
--         |> Json.Decode.Pipeline.required "homeTeam" decodeOptimisedTeam
--         |> Json.Decode.Pipeline.required "awayTeam" decodeOptimisedTeam
-- decodeOptimisedTeam : Json.Decode.Decoder OptimisedTeam
-- decodeOptimisedTeam =
--     Json.Decode.succeed OptimisedTeam
--         |> Json.Decode.Pipeline.required "name" Json.Decode.string
-- rest of model


type UiState
    = TeamsView
    | AddTeamView
    | EditTeamView
    | GameOrderView
    | AddGameView
    | EditGameView
    | OptimiseView


vanillaGame : Game
vanillaGame =
    Game
        vanillaTeam
        vanillaTeam



-- Nothing
-- Nothing
-- 0
-- encodeGame : Game -> Encode.Value
-- encodeGame game =
--     Encode.object
--         [ ( "HomeTeam", Encode.string game.homeTeam.name )
--         , ( "AwayTeam", Encode.string game.awayTeam.name )
--         ]
-- encodeGames : List Game -> Encode.Value
-- encodeGames games =
--     Encode.list encodeGame games


type Optimisation
    = NotStarted
    | InProgress GameOrderMetrics Int (Step Game)
    | Finished GameOrderMetrics Int


optimiseAllPermutations : List Game -> Optimisation -> Optimisation
optimiseAllPermutations games _ =
    let
        gameOrders =
            permutations curtailWhenTeamsPlayingConsecutively games

        bestGameOrder =
            List.foldr
                (\gameOrder currentBestGameOrderMetrics ->
                    let
                        gameOrderMetrics =
                            calculateGameOrderMetrics gameOrder
                    in
                    if
                        gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
                            < currentBestGameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
                            || (gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
                                    == currentBestGameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
                                    && gameOrderMetrics.tournamentPreferenceScore
                                    > currentBestGameOrderMetrics.tournamentPreferenceScore
                               )
                    then
                        gameOrderMetrics

                    else
                        currentBestGameOrderMetrics
                )
                { analysedGames = []
                , analysedTeams = []
                , occurencesOfTeamsPlayingConsecutiveGames = List.length games
                , tournamentPreferenceScore = 0
                }
                gameOrders
    in
    Finished bestGameOrder (factorial (List.length games))


factorial : Int -> Int
factorial n =
    List.product (List.range 1 n)


optimiseInChunks : List Game -> Optimisation -> Optimisation
optimiseInChunks games optimisation =
    case optimisation of
        NotStarted ->
            optimiseInChunks2
                { analysedGames = []
                , analysedTeams = []
                , occurencesOfTeamsPlayingConsecutiveGames = List.length games
                , tournamentPreferenceScore = 0
                }
                10000
                (first games)

        InProgress gameOrderMetrics maxPermutation step ->
            Debug.log "optimise2" (optimiseInChunks2 gameOrderMetrics (maxPermutation + 10000) step)

        Finished _ _ ->
            optimiseInChunks2
                { analysedGames = []
                , analysedTeams = []
                , occurencesOfTeamsPlayingConsecutiveGames = List.length games
                , tournamentPreferenceScore = 0
                }
                10000
                (first games)


optimiseInChunks2 : GameOrderMetrics -> Int -> Step Game -> Optimisation
optimiseInChunks2 currentBestGameOrderMetrics maxPermutation step =
    case step of
        Done ->
            -- todo store total permutations count, maybe in the state
            Finished currentBestGameOrderMetrics 0

        Next state permutation ->
            let
                gameOrderMetrics =
                    calculateGameOrderMetrics permutation

                nextBestGameOrderMetrics =
                    if
                        gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
                            < currentBestGameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
                            || (gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
                                    == currentBestGameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
                                    && gameOrderMetrics.tournamentPreferenceScore
                                    > currentBestGameOrderMetrics.tournamentPreferenceScore
                               )
                    then
                        gameOrderMetrics

                    else
                        currentBestGameOrderMetrics
            in
            if state.count >= maxPermutation then
                InProgress nextBestGameOrderMetrics maxPermutation step

            else
                optimiseInChunks2 nextBestGameOrderMetrics maxPermutation (next state)


type alias Model =
    { uiState : UiState
    , teams : List Team
    , games : List Game

    -- add team
    , teamNameToAdd : String

    -- edit team
    , teamToEdit : Team
    , editedTeamName : String

    -- add game
    , homeTeamToAdd : Team
    , awayTeamToAdd : Team

    -- edit game (might also want to allow change of order here, but probably it will happen another way)
    , gameToEdit : Game
    , editedHomeTeam : Team
    , editedAwayTeam : Team

    -- optimise
    , optimisation : Optimisation

    -- call to optimize api
    -- , optimisation : WebData Optimisation
    }


vanillaModel : Model
vanillaModel =
    Model
        TeamsView
        [ Team "Castle" TwoGamesRest
        , Team "ULU" FinishEarly
        , Team "Surrey" TwoGamesRest
        , Team "Braintree" TwoGamesRest
        , Team "VKC" TwoGamesRest
        , Team "St Albans" TwoGamesRest
        , Team "East End" StartLate
        ]
        [ Game (Team "ULU" FinishEarly) (Team "Braintree" TwoGamesRest)
        , Game (Team "ULU" FinishEarly) (Team "Surrey" TwoGamesRest)
        , Game (Team "ULU" FinishEarly) (Team "VKC" TwoGamesRest)
        , Game (Team "ULU" FinishEarly) (Team "St Albans" TwoGamesRest)
        , Game (Team "ULU" FinishEarly) (Team "East End" StartLate)
        , Game (Team "ULU" FinishEarly) (Team "Castle" TwoGamesRest)
        , Game (Team "Braintree" TwoGamesRest) (Team "Surrey" TwoGamesRest)
        , Game (Team "Braintree" TwoGamesRest) (Team "VKC" TwoGamesRest)
        , Game (Team "Braintree" TwoGamesRest) (Team "St Albans" TwoGamesRest)
        , Game (Team "Braintree" TwoGamesRest) (Team "East End" StartLate)
        , Game (Team "Braintree" TwoGamesRest) (Team "Castle" TwoGamesRest)
        , Game (Team "Surrey" TwoGamesRest) (Team "VKC" TwoGamesRest)
        , Game (Team "Surrey" TwoGamesRest) (Team "St Albans" TwoGamesRest)
        , Game (Team "Surrey" TwoGamesRest) (Team "East End" StartLate)
        , Game (Team "Surrey" TwoGamesRest) (Team "Castle" TwoGamesRest)
        , Game (Team "VKC" TwoGamesRest) (Team "St Albans" TwoGamesRest)
        , Game (Team "VKC" TwoGamesRest) (Team "East End" StartLate)
        , Game (Team "VKC" TwoGamesRest) (Team "Castle" TwoGamesRest)
        , Game (Team "St Albans" TwoGamesRest) (Team "East End" StartLate)
        , Game (Team "St Albans" TwoGamesRest) (Team "Castle" TwoGamesRest)
        , Game (Team "East End" StartLate) (Team "Castle" TwoGamesRest)
        ]
        ""
        vanillaTeam
        ""
        vanillaTeam
        vanillaTeam
        vanillaGame
        vanillaTeam
        vanillaTeam
        NotStarted


init : ( Model, Cmd Msg )
init =
    ( vanillaModel, Cmd.none )



---- UPDATE ----


type Msg
    = ShowTeams
      -- delete team
    | DeleteTeam Team
      -- add team
    | ShowAddTeam
    | SetTeamNameToAdd String
    | AddTeam
      -- edit team
    | ShowEditTeam Team
    | SetEditedTeamName String
    | EditTeam
      -- define games
    | ShowGameOrder
    | ShowAddGame
    | DeleteGame Game
      -- add game
    | SetHomeTeamNameToAdd String
    | SetAwayTeamNameToAdd String
    | AddGame
      -- edit game
    | ShowEditGame Game
    | SetEditedHomeTeam String
    | SetEditedAwayTeam String
    | EditGame
      -- Optimise
    | ShowOptimise
    | OptimiseGameOrder



-- | OptimiseGameOrderResponse (WebData Optimisation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Teams
        ShowTeams ->
            ( { model | uiState = TeamsView }, Cmd.none )

        DeleteTeam team ->
            ( { model | teams = List.Extra.remove team model.teams }, Cmd.none )

        -- Add team
        ShowAddTeam ->
            ( { model | uiState = AddTeamView }, Cmd.none )

        SetTeamNameToAdd teamName ->
            ( { model | teamNameToAdd = teamName }, Cmd.none )

        AddTeam ->
            ( { model | teams = Team model.teamNameToAdd TwoGamesRest :: model.teams, uiState = TeamsView }, Cmd.none )

        -- Edit team
        ShowEditTeam team ->
            ( { model | uiState = EditTeamView, teamToEdit = team, editedTeamName = team.name }, Cmd.none )

        SetEditedTeamName teamName ->
            ( { model | editedTeamName = teamName }, Cmd.none )

        EditTeam ->
            ( { model
                | teams =
                    List.map
                        --could use list.extra.updateif / setif here, its probably slightly better
                        (\t ->
                            if t == model.teamToEdit then
                                { t | name = model.editedTeamName }

                            else
                                t
                        )
                        model.teams
                , uiState = TeamsView
              }
            , Cmd.none
            )

        -- Game Order
        ShowGameOrder ->
            ( { model | uiState = GameOrderView, games = initializeGames model.teams model.games }, Cmd.none )

        DeleteGame game ->
            ( { model | games = List.Extra.remove game model.games }, Cmd.none )

        ShowAddGame ->
            ( { model | uiState = AddGameView, homeTeamToAdd = vanillaTeam, awayTeamToAdd = vanillaTeam }, Cmd.none )

        -- Add game
        SetHomeTeamNameToAdd teamName ->
            ( { model | homeTeamToAdd = List.Extra.find (\t -> t.name == teamName) model.teams |> Maybe.withDefault vanillaTeam }, Cmd.none )

        SetAwayTeamNameToAdd teamName ->
            ( { model | awayTeamToAdd = List.Extra.find (\t -> t.name == teamName) model.teams |> Maybe.withDefault vanillaTeam }, Cmd.none )

        AddGame ->
            let
                newModel =
                    { model
                        | games = Game model.homeTeamToAdd model.awayTeamToAdd :: model.games
                        , uiState = GameOrderView
                    }
                        |> clearOptimisationResults
            in
            ( newModel, Cmd.none )

        -- Edit game
        ShowEditGame game ->
            ( { model | uiState = EditGameView, gameToEdit = game, editedHomeTeam = game.homeTeam, editedAwayTeam = game.awayTeam }, Cmd.none )

        SetEditedHomeTeam homeTeamName ->
            ( { model | editedHomeTeam = List.Extra.find (\t -> t.name == homeTeamName) model.teams |> Maybe.withDefault vanillaTeam }, Cmd.none )

        SetEditedAwayTeam awayTeamName ->
            ( { model | editedAwayTeam = List.Extra.find (\t -> t.name == awayTeamName) model.teams |> Maybe.withDefault vanillaTeam }, Cmd.none )

        EditGame ->
            let
                gameToEdit =
                    model.gameToEdit

                newModel =
                    { model
                        | games =
                            List.Extra.setIf
                                ((==) gameToEdit)
                                { gameToEdit | homeTeam = model.editedHomeTeam, awayTeam = model.editedAwayTeam }
                                model.games
                        , uiState = GameOrderView
                    }
                        |> clearOptimisationResults
            in
            ( newModel
            , Cmd.none
            )

        -- Optimise
        ShowOptimise ->
            ( { model | uiState = OptimiseView }, Cmd.none )

        OptimiseGameOrder ->
            ( { model | optimisation = optimiseAllPermutations model.games model.optimisation }
              -- ( { model | optimisation = optimiseInChunks model.games model.optimisation }
            , Cmd.none
              -- , Http.post
              --     { url = "http://localhost:7071/api/OptimalGameOrder"
              --     , body = encodeGames model.games |> Http.jsonBody
              --     , expect = Http.expectJson (RemoteData.fromResult >> OptimiseGameOrderResponse) decodeOptimisation
              --     }
            )



-- OptimiseGameOrderResponse response ->
--     case response of
--         RemoteData.Success optimisation ->
--             ( { model
--                 | optimisation = response
--                 -- if we add other things to Team (such as whether they want to leave early)
--                 -- we will have to match up the OptimisedTeam's with the Team's
--                 , games = List.indexedMap (\index game -> Game (Team game.homeTeam.name TwoGamesRest) (Team game.awayTeam.name TwoGamesRest) (Just game.homeTeamPlayingConsecutively) (Just game.awayTeamPlayingConsecutively) index) optimisation.optimisedGameOrder.gameOrder
--               }
--             , Cmd.none
--             )
--         _ ->
--             ( { model | optimisation = response }
--             , Cmd.none
--             )


initializeGames : List Team -> List Game -> List Game
initializeGames teams existingGames =
    case existingGames of
        [] ->
            List.Extra.uniquePairs teams
                |> List.foldl (\( team1, team2 ) gameTuples -> ( team1, team2 ) :: ( team2, team1 ) :: gameTuples) []
                |> List.map (\( homeTeam, awayTeam ) -> Game homeTeam awayTeam)

        someGames ->
            someGames


clearOptimisationResults : Model -> Model
clearOptimisationResults model =
    { model | optimisation = NotStarted }



---- VIEW ----
-- maybe put class center in to css file if its ubiquitous
-- could have a stack = class "stack" function and similar as well


view : Model -> Html Msg
view model =
    div
        [ class "body stack" ]
        [ header
            []
            [ h1 [ class "text-center" ] [ text "Tournament Organiser" ]
            , h2 [ class "text-center" ] [ text "Optimise Order of Games" ]
            , nav
                [ class "center" ]
                [ -- this probably wants to enable / disable appropriately. not a biggie though
                  a
                    [ onClick ShowTeams ]
                    [ text "Define Teams" ]

                -- sort out the spans with some flex or something
                , span [] [ text " - " ]
                , -- this probably wants to enable / disable appropriately. not a biggie though
                  a
                    [ onClick ShowGameOrder ]
                    [ text "Define Games" ]
                , span [] [ text " - " ]
                , -- this probably wants to enable / disable appropriately. not a biggie though
                  a
                    [ onClick ShowOptimise ]
                    [ text "Optimise!" ]
                ]
            ]
        , main_
            [ class "center stack" ]
            (stateView model)
        ]


stateView : Model -> List (Html Msg)
stateView model =
    case model.uiState of
        TeamsView ->
            teamsView model

        AddTeamView ->
            addTeamView

        EditTeamView ->
            editTeamView model

        GameOrderView ->
            gameOrderView model

        AddGameView ->
            addGameView model

        EditGameView ->
            editGameView model

        OptimiseView ->
            optimiseView model


teamsView : Model -> List (Html Msg)
teamsView model =
    [ button
        [ onClick ShowAddTeam
        , class "primary center"
        ]
        [ text "Add Team" ]
    , ul
        [ class "stack stack-small" ]
        (List.map teamView model.teams)
    ]


teamView : Team -> Html Msg
teamView aTeam =
    li
        []
        [ span
            [ onClick (ShowEditTeam aTeam)
            , class "grow"
            ]
            [ text aTeam.name ]
        , deleteListItemButton (DeleteTeam aTeam)
        ]


addTeamView : List (Html Msg)
addTeamView =
    [ input
        [ onInput SetTeamNameToAdd ]
        []
    , button
        [ onClick AddTeam
        , class "primary center"
        ]
        [ text "Add Team" ]
    ]


editTeamView : Model -> List (Html Msg)
editTeamView model =
    [ input
        [ onInput SetEditedTeamName
        , Html.Attributes.value model.editedTeamName
        ]
        []
    , button
        [ onClick EditTeam ]
        [ text "Edit Team" ]
    ]


deleteIcon : Html msg
deleteIcon =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 448 512"
        , Svg.Attributes.class "icon"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.stroke "currentColor"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M135.2 17.7L128 32H32C14.3 32 0 46.3 0 64S14.3 96 32 96H416c17.7 0 32-14.3 32-32s-14.3-32-32-32H320l-7.2-14.3C307.4 6.8 296.3 0 284.2 0H163.8c-12.1 0-23.2 6.8-28.6 17.7zM416 128H32L53.2 467c1.6 25.3 22.6 45 47.9 45H346.9c25.3 0 46.3-19.7 47.9-45L416 128z"
            ]
            []
        ]


editIcon : Html msg
editIcon =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 512 512"
        , Svg.Attributes.class "icon"
        , Svg.Attributes.fill "currentColor"
        , Svg.Attributes.stroke "currentColor"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M471.6 21.7c-21.9-21.9-57.3-21.9-79.2 0L362.3 51.7l97.9 97.9 30.1-30.1c21.9-21.9 21.9-57.3 0-79.2L471.6 21.7zm-299.2 220c-6.1 6.1-10.8 13.6-13.5 21.9l-29.6 88.8c-2.9 8.6-.6 18.1 5.8 24.6s15.9 8.7 24.6 5.8l88.8-29.6c8.2-2.8 15.7-7.4 21.9-13.5L437.7 172.3 339.7 74.3 172.4 241.7zM96 64C43 64 0 107 0 160V416c0 53 43 96 96 96H352c53 0 96-43 96-96V320c0-17.7-14.3-32-32-32s-32 14.3-32 32v96c0 17.7-14.3 32-32 32H96c-17.7 0-32-14.3-32-32V160c0-17.7 14.3-32 32-32h96c17.7 0 32-14.3 32-32s-14.3-32-32-32H96z"
            ]
            []
        ]


deleteListItemButton : msg -> Html msg
deleteListItemButton onDelete =
    button
        [ onClick onDelete
        , class "list"
        ]
        [ deleteIcon ]


editListItemButton : msg -> Html msg
editListItemButton onEdit =
    button
        [ onClick onEdit
        , class "list"
        ]
        [ editIcon ]


gameOrderView : Model -> List (Html Msg)
gameOrderView model =
    [ button
        [ onClick ShowAddGame
        , class "primary center"
        ]
        [ text "Add Game" ]
    , ul
        [ class "stack stack-small" ]
        (List.map gameView model.games)
    ]


gameView : Game -> Html Msg
gameView game =
    li
        []
        [ span
            [ class "grow" ]
            [ span
                []
                [ text game.homeTeam.name ]
            , span
                []
                [ text " - " ]
            , span
                []
                [ text game.awayTeam.name ]
            ]
        , editListItemButton (ShowEditGame game)

        -- use some styling here instead of the separator
        , span [] [ text "\u{00A0}" ]
        , deleteListItemButton (DeleteGame game)
        ]



-- optimiseView : Model -> List (Html Msg)
-- optimiseView model =
-- optimiseView : Model -> List (Html Msg)
-- optimiseView model =
--     case model.optimisation of
--         RemoteData.Success optimisation ->
--             p [] [ text optimisation.optimisationMessage ] :: optimisedGameOrderView model
--         RemoteData.NotAsked ->
--             optimisedGameOrderView model
--         RemoteData.Loading ->
--             [ loading ]
--         RemoteData.Failure error ->
--             p [] [ text (httpErrorMessage error) ] :: optimisedGameOrderView model
-- type Optimisation
--     = NotStarted
--     | InProgress GameOrderMetrics Int (Step Game)
--     | Finished GameOrderMetrics Int


optimiseView : Model -> List (Html Msg)
optimiseView model =
    button
        [ onClick OptimiseGameOrder
        , class "primary center"
        ]
        [ text "Optimise" ]
        :: optimisationView model.optimisation


optimisationView : Optimisation -> List (Html Msg)
optimisationView optimisation =
    case optimisation of
        NotStarted ->
            [ p [] [ text "Please click the 'Optimise' button" ] ]

        InProgress gameOrderMetrics permutationsAnalysed _ ->
            [ p [] [ text "In progress ..." ]
            , optimisedGamesView gameOrderMetrics.analysedGames
            ]

        Finished gameOrderMetrics totalPermutations ->
            [ p [] [ text "Finished" ]
            , optimisedGamesView gameOrderMetrics.analysedGames
            ]


optimisedGamesView : List AnalysedGame -> Html Msg
optimisedGamesView analysedGames =
    ul
        [ class "stack stack-small" ]
        (List.map optimisedGameView analysedGames)


optimisedGameView : AnalysedGame -> Html Msg
optimisedGameView game =
    li
        [ class "center" ]
        [ span
            (consecutiveGameClass game.homeTeamPlayingConsecutively)
            [ text game.homeTeam.name ]
        , span
            []
            [ text "\u{00A0}-\u{00A0}" ]
        , span
            (consecutiveGameClass game.awayTeamPlayingConsecutively)
            [ text game.awayTeam.name ]
        ]


consecutiveGameClass : Bool -> List (Html.Attribute Msg)
consecutiveGameClass playingConsecutiveley =
    if playingConsecutiveley then
        [ class "playingConsecutively" ]

    else
        [ class "atLeastOneGameToRecover" ]


addGameView : Model -> List (Html Msg)
addGameView model =
    [ select
        [ onInput SetHomeTeamNameToAdd ]
        (option [] [ text "Please select team" ] :: List.map (\t -> option [] [ text t.name ]) model.teams)
    , p
        [ class "text-center" ]
        [ text " versus " ]
    , select
        [ onInput SetAwayTeamNameToAdd ]
        (option [] [ text "Please select team" ] :: List.map (\t -> option [] [ text t.name ]) model.teams)
    , button
        [ onClick AddGame
        , class "primary center"
        ]
        [ text "Add Game" ]
    ]


editGameView : Model -> List (Html Msg)
editGameView model =
    [ select
        [ onInput SetEditedHomeTeam ]
        (option [] [ text "Please select team" ] :: List.map (\t -> option [ Html.Attributes.selected (t == model.editedHomeTeam) ] [ text t.name ]) model.teams)
    , p
        [ class "text-center" ]
        [ text " versus " ]
    , select
        [ onInput SetEditedAwayTeam
        , Html.Attributes.value model.editedAwayTeam.name
        ]
        (option [] [ text "Please select team" ] :: List.map (\t -> option [ Html.Attributes.selected (t == model.editedAwayTeam) ] [ text t.name ]) model.teams)
    , button
        [ onClick EditGame
        , class "primary center"
        ]
        [ text "Edit Game" ]
    ]


loading : Html msg
loading =
    Html.div
        [ Html.Attributes.class "loading" ]
        [ Html.div
            [ Html.Attributes.class "la-ball-newton-cradle la-3x" ]
            [ Html.div [] []
            , Html.div [] []
            , Html.div [] []
            , Html.div [] []
            ]
        ]



-- httpErrorMessage : Http.Error -> String
-- httpErrorMessage error =
--     case error of
--         Http.Timeout ->
--             "Timeout, maybe there is no connection to the internet, or the optimisation server is down"
--         Http.NetworkError ->
--             "Network error, maybe there is no connection to the internet, or the optimisation server is down"
--         Http.BadBody message ->
--             "Unexpected Body. Hmmm, there is probably a problem in my configuration, please create an issue. Error returned: " ++ message
--         Http.BadStatus statusCode ->
--             "Bad Response. Hmmm, there is probably a problem in my configuration, or there is a problem with the optimisation server. Please create an issue. Status returned: " ++ String.fromInt statusCode
--         Http.BadUrl message ->
--             "Bad Url. Hmmm, there is probably a problem in my configuration, please create an issue. Error returned: " ++ message
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
