port module Main exposing (..)

import Browser
import GameParser
import Html exposing (Html, a, button, div, h1, h2, header, input, li, main_, nav, option, p, select, span, text, ul)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Optimisation.GameOrderMetrics exposing (AnalysedGame, Game, GameOrderMetrics, Team, TournamentPreference(..), bestGameOrderMetrics, calculateGameOrderMetrics, curtailWhenTeamsPlayingConsecutively, neverCurtail, optimiseAllPermutations, vanillaTeam)
import Parser
import String
import Svg
import Svg.Attributes



---- Ports ----


port paste : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    paste
        (\pastedString ->
            case Parser.run GameParser.gamesParser pastedString of
                Ok games ->
                    -- todo - add games to model, and add any new teams we didn't already know about
                    PasteGames games

                Err _ ->
                    -- todo, probably show a message here
                    PasteGames []
        )



---- MODEL ----


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


factorial : Int -> Int
factorial n =
    List.product (List.range 1 n)



-- optimiseInChunks : List Game -> Optimisation -> Optimisation
-- optimiseInChunks games optimisation =
--     case optimisation of
--         NotStarted ->
--             optimiseInChunks2
--                 { analysedGames = []
--                 , analysedTeams = []
--                 , occurencesOfTeamsPlayingConsecutiveGames = List.length games
--                 , tournamentPreferenceScore = 0
--                 }
--                 10000
--                 (first games)
--         InProgress gameOrderMetrics maxPermutation step ->
--             Debug.log "optimise2" (optimiseInChunks2 gameOrderMetrics (maxPermutation + 10000) step)
--         Finished _ _ ->
--             optimiseInChunks2
--                 { analysedGames = []
--                 , analysedTeams = []
--                 , occurencesOfTeamsPlayingConsecutiveGames = List.length games
--                 , tournamentPreferenceScore = 0
--                 }
--                 10000
--                 (first games)
-- optimiseInChunks2 : GameOrderMetrics -> Int -> Step Game -> Optimisation
-- optimiseInChunks2 currentBestGameOrderMetrics maxPermutation step =
--     case step of
--         Done ->
--             -- todo store total permutations count, maybe in the state
--             Finished currentBestGameOrderMetrics 0
--         Next state permutation ->
--             let
--                 gameOrderMetrics =
--                     calculateGameOrderMetrics permutation
--                 nextBestGameOrderMetrics =
--                     if
--                         gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
--                             < currentBestGameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
--                             || (gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
--                                     == currentBestGameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames
--                                     && gameOrderMetrics.tournamentPreferenceScore
--                                     > currentBestGameOrderMetrics.tournamentPreferenceScore
--                                )
--                     then
--                         gameOrderMetrics
--                     else
--                         currentBestGameOrderMetrics
--             in
--             if state.count >= maxPermutation then
--                 InProgress nextBestGameOrderMetrics maxPermutation step
--             else
--                 optimiseInChunks2 nextBestGameOrderMetrics maxPermutation (next state)


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
    , gameOrderMetrics : Maybe GameOrderMetrics
    , previousGameOrderMetrics : Maybe GameOrderMetrics
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
        Nothing
        Nothing


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
      -- Paste
    | PasteGames (List Game)


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
            let
                games =
                    Maybe.map (\gameOrderMetrics -> List.map .game gameOrderMetrics.analysedGames) model.gameOrderMetrics
                        |> Maybe.withDefault model.games
            in
            ( { model
                | previousGameOrderMetrics = model.gameOrderMetrics
                , gameOrderMetrics = Just (optimiseAllPermutations games model.gameOrderMetrics)
              }
              -- ( { model | optimisation = optimiseInChunks model.games model.optimisation }
            , Cmd.none
            )

        -- Paste
        PasteGames games ->
            let
                _ =
                    Debug.log "paste" games
            in
            ( model, Cmd.none )


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
    { model | gameOrderMetrics = Nothing }



---- VIEW ----
-- maybe put class center in to css file if its ubiquitous
-- could have a stack = class "stack" function and similar as well


view : Model -> Html Msg
view model =
    div
        [ class "body stack" ]
        [ header
            []
            [ h1 [] [ text "Tournament Organiser" ]
            , h2 [] [ text "Optimise Order of Games" ]
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
        , editListItemButton (ShowEditTeam aTeam)

        -- use some styling here instead of the separator
        , span [] [ text "\u{00A0}" ]
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
        [ onClick EditTeam
        , class "primary center"
        ]
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


optimiseView : Model -> List (Html Msg)
optimiseView model =
    let
        optimiseDisabled =
            case ( model.previousGameOrderMetrics, model.gameOrderMetrics ) of
                ( Nothing, _ ) ->
                    False

                ( previousGameOrderMetrics, gameOrderMetrics ) ->
                    previousGameOrderMetrics == gameOrderMetrics
    in
    button
        [ onClick OptimiseGameOrder
        , class "primary center"
        , disabled optimiseDisabled
        ]
        [ text "Optimise" ]
        :: optimisationView model.previousGameOrderMetrics model.gameOrderMetrics


optimisationView : Maybe GameOrderMetrics -> Maybe GameOrderMetrics -> List (Html Msg)
optimisationView previousGameOrderMetrics maybeGameOrderMetrics =
    case maybeGameOrderMetrics of
        Nothing ->
            [ p [] [ text "Please click the 'Optimise' button" ] ]

        Just gameOrderMetrics ->
            optimisationExplanation previousGameOrderMetrics gameOrderMetrics
                ++ [ optimisedGamesView gameOrderMetrics.analysedGames
                   ]


optimisationExplanation : Maybe GameOrderMetrics -> GameOrderMetrics -> List (Html Msg)
optimisationExplanation previousGameOrderMetrics gameOrderMetrics =
    if gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames > 0 then
        [ p [] [ text "Could not find any game orders where teams did not play back to back, showing the best result I could find." ] ]

    else if Just gameOrderMetrics /= previousGameOrderMetrics then
        [ p [] [ text "Showing the best game order I found so far. You can click 'Optimise' again to analyse more options." ]
        , preferenceExplanation gameOrderMetrics
        ]

    else
        [ p [] [ text "Showing the best game order I could find." ]
        , preferenceExplanation gameOrderMetrics
        ]


preferenceExplanation : GameOrderMetrics -> Html Msg
preferenceExplanation gameOrderMetrics =
    let
        lowestTournamentPreferenceScore =
            gameOrderMetrics.lowestTournamentPreferenceScore
                * 100
                |> floor
                |> String.fromInt

        highestTournamentPreferenceScore =
            gameOrderMetrics.highestTournamentPreferenceScore
                * 100
                |> floor
                |> String.fromInt
    in
    p
        []
        [ text
            ("The team preferences are accommodated with "
                ++ lowestTournamentPreferenceScore
                ++ "-"
                ++ highestTournamentPreferenceScore
                ++ "% success"
            )
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
            [ text game.game.homeTeam.name ]
        , span
            []
            [ text "\u{00A0}-\u{00A0}" ]
        , span
            (consecutiveGameClass game.awayTeamPlayingConsecutively)
            [ text game.game.awayTeam.name ]
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


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
