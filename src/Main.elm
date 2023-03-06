port module Main exposing (Model, Msg(..), UiState(..), main)

import Browser
import GameParser
import Html exposing (Html, a, button, datalist, div, h1, h2, header, input, li, main_, nav, option, p, select, span, text, ul)
import Html.Attributes exposing (class, disabled, id, list, placeholder, selected, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Optimisation.GameOrderMetrics exposing (AnalysedGame, Game, GameOrderMetrics, Team, TournamentPreference(..), optimiseAllPermutations, playing, tournamentPreferenceFromString, tournamentPreferenceToString, vanillaGame, vanillaTeam)
import Parser
import String
import Svg
import Svg.Attributes



---- Ports ----


port copyAnalysedGames : String -> Cmd msg


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
    = GamesView
    | AddGameView
    | EditGameView
    | TeamsView
    | OptimiseView





type alias Model =
    { uiState : UiState
    , teams : List Team
    , games : List Game

    -- add game
    , homeTeamToAdd : Team
    , awayTeamToAdd : Team

    -- edit game
    , gameToEdit : Game
    , editedHomeTeam : Team
    , editedAwayTeam : Team

    -- optimise
    , gameOrderMetrics : Maybe GameOrderMetrics
    , previousGameOrderMetrics : Maybe GameOrderMetrics
    }


exampleGames : List Game
exampleGames =
    [ Game (Team "ULU" TwoGamesRest) (Team "Braintree" TwoGamesRest)
    , Game (Team "ULU" TwoGamesRest) (Team "Surrey" TwoGamesRest)
    , Game (Team "ULU" TwoGamesRest) (Team "VKC" TwoGamesRest)
    , Game (Team "ULU" TwoGamesRest) (Team "St Albans" TwoGamesRest)
    , Game (Team "ULU" TwoGamesRest) (Team "East End" TwoGamesRest)
    , Game (Team "ULU" TwoGamesRest) (Team "Castle" TwoGamesRest)
    , Game (Team "Braintree" TwoGamesRest) (Team "Surrey" TwoGamesRest)
    , Game (Team "Braintree" TwoGamesRest) (Team "VKC" TwoGamesRest)
    , Game (Team "Braintree" TwoGamesRest) (Team "St Albans" TwoGamesRest)
    , Game (Team "Braintree" TwoGamesRest) (Team "East End" TwoGamesRest)
    , Game (Team "Braintree" TwoGamesRest) (Team "Castle" TwoGamesRest)
    , Game (Team "Surrey" TwoGamesRest) (Team "VKC" TwoGamesRest)
    , Game (Team "Surrey" TwoGamesRest) (Team "St Albans" TwoGamesRest)
    , Game (Team "Surrey" TwoGamesRest) (Team "East End" TwoGamesRest)
    , Game (Team "Surrey" TwoGamesRest) (Team "Castle" TwoGamesRest)
    , Game (Team "VKC" TwoGamesRest) (Team "St Albans" TwoGamesRest)
    , Game (Team "VKC" TwoGamesRest) (Team "East End" TwoGamesRest)
    , Game (Team "VKC" TwoGamesRest) (Team "Castle" TwoGamesRest)
    , Game (Team "St Albans" TwoGamesRest) (Team "East End" TwoGamesRest)
    , Game (Team "St Albans" TwoGamesRest) (Team "Castle" TwoGamesRest)
    , Game (Team "East End" TwoGamesRest) (Team "Castle" TwoGamesRest)
    ]


vanillaModel : Model
vanillaModel =
    Model
        GamesView
        []
        []
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
      -- define games
    | ShowGames
    | ShowAddGame
    | DeleteGame Game
    | AddExampleGames
      -- add game
    | SetHomeTeamNameToAdd String
    | SetAwayTeamNameToAdd String
    | AddGame
      -- edit game
    | ShowEditGame Game
    | SetEditedHomeTeam String
    | SetEditedAwayTeam String
    | EditGame
      -- edit team
    | EditTournamentPreference Team TournamentPreference
      -- Optimise
    | ShowOptimise
    | OptimiseGameOrder
      -- Copy / Paste
    | CopyOptimisedGames (List AnalysedGame)
    | PasteGames (List Game)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Games
        ShowGames ->
            ( { model | uiState = GamesView }, Cmd.none )

        DeleteGame game ->
            let
                newModel : Model
                newModel =
                    setGames (List.Extra.remove game model.games)
                        { model | uiState = GamesView }
            in
            ( newModel, Cmd.none )

        ShowAddGame ->
            ( { model | uiState = AddGameView, homeTeamToAdd = vanillaTeam, awayTeamToAdd = vanillaTeam }, Cmd.none )

        AddExampleGames ->
            ( setGames exampleGames model, Cmd.none )

        -- Add game
        SetHomeTeamNameToAdd teamName ->
            ( { model | homeTeamToAdd = List.Extra.find (\t -> t.name == teamName) model.teams |> Maybe.withDefault (Team teamName NoPreference) }, Cmd.none )

        SetAwayTeamNameToAdd teamName ->
            ( { model | awayTeamToAdd = List.Extra.find (\t -> t.name == teamName) model.teams |> Maybe.withDefault (Team teamName NoPreference) }, Cmd.none )

        AddGame ->
            let
                newModel : Model
                newModel =
                    setGames (Game model.homeTeamToAdd model.awayTeamToAdd :: model.games)
                        { model | uiState = GamesView }
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
                gameToEdit : Game
                gameToEdit =
                    model.gameToEdit

                newGames : List Game
                newGames =
                    List.Extra.setIf
                        ((==) gameToEdit)
                        { gameToEdit | homeTeam = model.editedHomeTeam, awayTeam = model.editedAwayTeam }
                        model.games

                newModel : Model
                newModel =
                    setGames newGames { model | uiState = GamesView }
            in
            ( newModel
            , Cmd.none
            )

        -- Teams
        ShowTeams ->
            ( { model | uiState = TeamsView }, Cmd.none )

        EditTournamentPreference teamToEdit tournamentPreference ->
            let
                newTeam : Team
                newTeam =
                    { teamToEdit | tournamentPreference = tournamentPreference }

                editedTeams : List Team
                editedTeams =
                    List.Extra.updateIf
                        ((==) teamToEdit)
                        (\_ -> newTeam)
                        model.teams

                editedGames : List { a | awayTeam : Team, homeTeam : Team }
                editedGames =
                    List.map
                        (\game ->
                            let
                                homeTeam : Team
                                homeTeam =
                                    if game.homeTeam == teamToEdit then
                                        newTeam

                                    else
                                        game.homeTeam

                                awayTeam : Team
                                awayTeam =
                                    if game.awayTeam == teamToEdit then
                                        newTeam

                                    else
                                        game.awayTeam
                            in
                            { game | awayTeam = awayTeam, homeTeam = homeTeam }
                        )
                        model.games
            in
            ( setGames editedGames { model | teams = editedTeams }
            , Cmd.none
            )

        -- Optimise
        ShowOptimise ->
            ( { model | uiState = OptimiseView }, Cmd.none )

        OptimiseGameOrder ->
            let
                games : List Game
                games =
                    Maybe.map (\gameOrderMetrics -> List.map .game gameOrderMetrics.analysedGames) model.gameOrderMetrics
                        |> Maybe.withDefault model.games
            in
            ( { model
                | previousGameOrderMetrics = model.gameOrderMetrics
                , gameOrderMetrics = Just (optimiseAllPermutations games model.gameOrderMetrics)
              }
            , Cmd.none
            )

        -- Copy / Paste
        CopyOptimisedGames analysedGames ->
            ( model
            , copyAnalysedGames
                (List.map
                    (\analysedGame -> analysedGame.game.homeTeam.name ++ "\t" ++ analysedGame.game.awayTeam.name)
                    analysedGames
                    |> String.join "\n"
                )
            )

        PasteGames games ->
            if model.uiState == GamesView then
                ( setGames games model, Cmd.none )

            else
                ( model, Cmd.none )


clearOptimisationResults : Model -> Model
clearOptimisationResults model =
    { model | previousGameOrderMetrics = Nothing, gameOrderMetrics = Nothing }


setGames : List Game -> Model -> Model
setGames games model =
    let
        oldTeams : List Team
        oldTeams =
            List.filter (\team -> List.any (\game -> playing team game) games) model.teams

        newTeams : List { name : String, tournamentPreference : TournamentPreference }
        newTeams =
            List.concatMap (\game -> [ game.homeTeam, game.awayTeam ]) games
                |> List.Extra.unique
                |> List.filter (\newTeam -> not (List.any (\oldTeam -> oldTeam.name == newTeam.name) oldTeams))
    in
    { model | games = games, teams = oldTeams ++ newTeams }
        |> clearOptimisationResults



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
                -- sort out the spans with some flex or something
                [ a
                    [ onClick ShowGames ]
                    [ text "Define Games" ]
                , span [] [ text " - " ]
                , if List.isEmpty model.games then
                    span [] [ text "Team Options" ]

                  else
                    a
                        [ onClick ShowTeams, disabled (List.isEmpty model.games) ]
                        [ text "Team Options" ]
                , span [] [ text " - " ]
                , if List.isEmpty model.games then
                    span [] [ text "Optimise!" ]

                  else
                    a
                        [ onClick ShowOptimise, disabled (List.isEmpty model.games) ]
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
        GamesView ->
            gamesView model

        AddGameView ->
            addGameView model

        EditGameView ->
            editGameView model

        TeamsView ->
            teamsView model

        OptimiseView ->
            optimiseView model


gamesView : Model -> List (Html Msg)
gamesView model =
    button
        [ onClick ShowAddGame
        , class "primary center"
        ]
        [ text "Add Game" ]
        :: gamesView2 model


gamesView2 : Model -> List (Html Msg)
gamesView2 model =
    if List.length model.games > 0 then
        [ ul
            [ class "stack stack-small" ]
            (List.map gameView model.games)
        ]

    else
        gamesViewEmptyState


gamesViewEmptyState : List (Html Msg)
gamesViewEmptyState =
    [ p [] [ text "You can copy / paste games from a spreadsheet here. There should be two columns, one for each team." ]
    , p [] [ text "You can also click on the 'Add Game' button to add games manually." ]
    , p [] [ text "If you just want to play around, click the button below to add some example games." ]
    , button
        [ onClick AddExampleGames
        , class "secondary center"
        ]
        [ text "Add Example Games" ]
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


addGameView : Model -> List (Html Msg)
addGameView model =
    [ teamsDataList model
    , input
        [ onInput SetHomeTeamNameToAdd
        , list "teamsDataList"
        , placeholder "Please enter or select team"
        ]
        []
    , p
        [ class "text-center" ]
        []
    , input
        [ onInput SetAwayTeamNameToAdd
        , list "teamsDataList"
        , placeholder "Please enter or select team"
        ]
        []
    , button
        [ onClick AddGame
        , class "primary center"
        ]
        [ text "Add Game" ]
    ]


editGameView : Model -> List (Html Msg)
editGameView model =
    [ teamsDataList model
    , input
        [ onInput SetEditedHomeTeam
        , list "teamsDataList"
        , placeholder "Please select team"
        ]
        []
    , p
        [ class "text-center" ]
        [ text " versus " ]
    , input
        [ onInput SetEditedAwayTeam
        , list "teamsDataList"
        , placeholder "Please select team"
        ]
        []
    , button
        [ onClick EditGame
        , class "primary center"
        ]
        [ text "Edit Game" ]
    ]


teamsDataList : Model -> Html Msg
teamsDataList model =
    datalist
        [ id "teamsDataList" ]
        (List.map (\team -> option [ value team.name ] []) model.teams)


teamsView : Model -> List (Html Msg)
teamsView model =
    [ ul
        [ class "stack stack-small" ]
        (List.map teamView model.teams)
    ]


teamView : Team -> Html Msg
teamView team =
    li
        []
        [ span
            [ class "grow" ]
            [ text team.name ]
        , tournamentPreferenceSelector team
        ]


tournamentPreferenceSelector : Team -> Html Msg
tournamentPreferenceSelector team =
    select
        [ onInput (\s -> EditTournamentPreference team (tournamentPreferenceFromString s)) ]
        [ tournamentPreferenceOption team NoPreference
        , tournamentPreferenceOption team StartLate
        , tournamentPreferenceOption team FinishEarly
        , tournamentPreferenceOption team TwoGamesRest
        ]


tournamentPreferenceOption : Team -> TournamentPreference -> Html Msg
tournamentPreferenceOption team tournamentPreference =
    option
        [ selected (team.tournamentPreference == tournamentPreference) ]
        [ text <| tournamentPreferenceToString tournamentPreference ]



optimiseView : Model -> List (Html Msg)
optimiseView model =
    let
        optimiseDisabled : Bool
        optimiseDisabled =
            case ( model.previousGameOrderMetrics, model.gameOrderMetrics ) of
                ( Nothing, _ ) ->
                    False

                ( previousGameOrderMetrics, gameOrderMetrics ) ->
                    previousGameOrderMetrics
                        == gameOrderMetrics
                        || Maybe.map .lowestTournamentPreferenceScore gameOrderMetrics
                        == Just 1

        copyDisabled : Bool
        copyDisabled =
            model.gameOrderMetrics == Nothing

        analysedGames : List AnalysedGame
        analysedGames =
            Maybe.map .analysedGames model.gameOrderMetrics |> Maybe.withDefault []
    in
    button
        [ onClick OptimiseGameOrder
        , class "primary center"
        , disabled optimiseDisabled
        ]
        [ text "Optimise" ]
        :: button
            [ onClick (CopyOptimisedGames analysedGames)
            , class "primary center"
            , disabled copyDisabled
            ]
            [ text "Copy to clipboard" ]
        :: optimisationView model.previousGameOrderMetrics model.gameOrderMetrics


optimisationView : Maybe GameOrderMetrics -> Maybe GameOrderMetrics -> List (Html Msg)
optimisationView previousGameOrderMetrics maybeGameOrderMetrics =
    case maybeGameOrderMetrics of
        Nothing ->
            [ p [] [ text "Please click the 'Optimise' button" ] ]

        Just gameOrderMetrics ->
            optimisationExplanation previousGameOrderMetrics gameOrderMetrics
                ++ [ optimisedGamesView gameOrderMetrics.analysedGames ]


optimisationExplanation : Maybe GameOrderMetrics -> GameOrderMetrics -> List (Html Msg)
optimisationExplanation previousGameOrderMetrics gameOrderMetrics =
    if gameOrderMetrics.occurencesOfTeamsPlayingConsecutiveGames > 0 then
        [ p [] [ text "Could not find any game orders where teams did not play back to back, showing the best result I could find." ] ]

    else if gameOrderMetrics.lowestTournamentPreferenceScore == 1 then
        [ p [] [ text "Showing the best game order, the team preferences are accommodated with 100% success" ] ]

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
        lowestTournamentPreferenceScore : String
        lowestTournamentPreferenceScore =
            gameOrderMetrics.lowestTournamentPreferenceScore
                * 100
                |> floor
                |> String.fromInt

        highestTournamentPreferenceScore : String
        highestTournamentPreferenceScore =
            gameOrderMetrics.highestTournamentPreferenceScore
                * 100
                |> floor
                |> String.fromInt

        explanation : String
        explanation =
            "The team preferences are accommodated with "
                ++ lowestTournamentPreferenceScore
                ++ "-"
                ++ highestTournamentPreferenceScore
                ++ "% success"
    in
    p [] [ text explanation ]


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


deleteListItemButton : msg -> Html msg
deleteListItemButton onDelete =
    button
        [ onClick onDelete ]
        [ deleteIcon ]


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


editListItemButton : msg -> Html msg
editListItemButton onEdit =
    button
        [ onClick onEdit ]
        [ editIcon ]


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


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
