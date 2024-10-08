port module Main exposing (Model, Msg(..), UiState(..), main)

import Browser
import Browser.Navigation
import DnDList
import GameParser
import Html exposing (Html, a, button, datalist, div, footer, h1, h2, h3, header, input, li, main_, nav, option, p, select, span, strong, sup, text, ul)
import Html.Attributes exposing (class, disabled, href, id, list, placeholder, selected, title, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import List.Extra
import Optimisation.GameOrderMetrics exposing (AnalysedGame, Game, GameOrderMetrics, Team, TournamentPreference(..), calculateGameOrderMetrics, decodeGame, decodeGameOrderMetrics, decodeTeam, encodeGame, encodeGameOrderMetrics, encodeTeam, initialSortOrderForGame, optimiseAllPermutations, playing, stringToTournamentPreference, tournamentPreferenceToString, vanillaGame, vanillaTeam)
import Parser
import Process
import String exposing (fromInt)
import Svg
import Svg.Attributes
import Task
import Url



---- Ports ----


port copyAnalysedGames : String -> Cmd msg


port paste : (String -> msg) -> Sub msg


port saveToLocalStorage : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ paste
            (\pastedString ->
                case Parser.run GameParser.gamesParser pastedString of
                    Ok games ->
                        -- todo - add games to model, and add any new teams we didn't already know about
                        PasteGames games

                    Err _ ->
                        -- todo, probably show a message here
                        PasteGames []
            )
        , system.subscriptions model.dnd
        ]



---- Ports for drag and drop ----


port onPointerMove : (Json.Encode.Value -> msg) -> Sub msg


port onPointerUp : (Json.Encode.Value -> msg) -> Sub msg


port releasePointerCapture : Json.Encode.Value -> Cmd msg



---- Drag and drop ----


config : DnDList.Config AnalysedGame
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system : DnDList.System AnalysedGame Msg
system =
    DnDList.createWithTouch config DndMsg onPointerMove onPointerUp releasePointerCapture



---- MODEL ----


type UiState
    = GamesView
    | AddGameView
    | EditGameView
    | TeamsView
    | TeamOptionsExplanationView
    | OptimiseView
    | TweakView


type alias Model =
    -- could probably do some better work here to make invalid states unrepresentable
    { key : Browser.Navigation.Key
    , uiState : UiState

    -- Games
    , games : List Game

    -- - add game
    , homeTeamToAdd : Team
    , awayTeamToAdd : Team

    -- - edit game
    , gameToEdit : Game
    , homeTeamNameToEdit : String
    , awayTeamNameToEdit : String

    -- teams
    , teams : List Team

    -- optimise
    , gameOrderMetrics : Maybe GameOrderMetrics
    , previousGameOrderMetrics : Maybe GameOrderMetrics
    , optimising : Bool

    -- tweak
    , dnd : DnDList.Model
    , tweakedGameOrderMetrics : Maybe GameOrderMetrics
    }


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "games", Json.Encode.list encodeGame model.games )
        , ( "teams", Json.Encode.list encodeTeam model.teams )
        , ( "gameOrderMetrics", Maybe.map encodeGameOrderMetrics model.gameOrderMetrics |> Maybe.withDefault Json.Encode.null )
        , ( "previousGameOrderMetrics", Maybe.map encodeGameOrderMetrics model.previousGameOrderMetrics |> Maybe.withDefault Json.Encode.null )
        , ( "tweakedGameOrderMetrics", Maybe.map encodeGameOrderMetrics model.tweakedGameOrderMetrics |> Maybe.withDefault Json.Encode.null )
        ]


decodeModel : Model -> Json.Decode.Decoder Model
decodeModel vanillaModelWithKey =
    Json.Decode.map5 (localStorageModelToModel vanillaModelWithKey)
        (Json.Decode.field "games" (Json.Decode.list decodeGame))
        (Json.Decode.field "teams" (Json.Decode.list decodeTeam))
        (Json.Decode.field "gameOrderMetrics" decodeGameOrderMetrics)
        (Json.Decode.field "previousGameOrderMetrics" decodeGameOrderMetrics)
        (Json.Decode.field "tweakedGameOrderMetrics" decodeGameOrderMetrics)


localStorageModelToModel : Model -> List Game -> List Team -> Maybe GameOrderMetrics -> Maybe GameOrderMetrics -> Maybe GameOrderMetrics -> Model
localStorageModelToModel vanillaModelWithKey games teams gameOrderMetrics previousGameOrderMetrics tweakedGameOrderMetrics =
    { vanillaModelWithKey
        | games = games
        , teams = teams
        , gameOrderMetrics = gameOrderMetrics
        , previousGameOrderMetrics = previousGameOrderMetrics
        , tweakedGameOrderMetrics = tweakedGameOrderMetrics
    }


vanillaModel : Browser.Navigation.Key -> Model
vanillaModel key =
    Model
        key
        GamesView
        []
        vanillaTeam
        vanillaTeam
        vanillaGame
        ""
        ""
        []
        Nothing
        Nothing
        False
        system.model
        Nothing


exampleGames : List Game
exampleGames =
    [ Game (Team "ULU" EvenlySpaced) (Team "Braintree" EvenlySpaced)
    , Game (Team "ULU" EvenlySpaced) (Team "Surrey" EvenlySpaced)
    , Game (Team "ULU" EvenlySpaced) (Team "VKC" EvenlySpaced)
    , Game (Team "ULU" EvenlySpaced) (Team "St Albans" EvenlySpaced)
    , Game (Team "ULU" EvenlySpaced) (Team "East End" EvenlySpaced)
    , Game (Team "ULU" EvenlySpaced) (Team "Castle" EvenlySpaced)
    , Game (Team "Braintree" EvenlySpaced) (Team "Surrey" EvenlySpaced)
    , Game (Team "Braintree" EvenlySpaced) (Team "VKC" EvenlySpaced)
    , Game (Team "Braintree" EvenlySpaced) (Team "St Albans" EvenlySpaced)
    , Game (Team "Braintree" EvenlySpaced) (Team "East End" EvenlySpaced)
    , Game (Team "Braintree" EvenlySpaced) (Team "Castle" EvenlySpaced)
    , Game (Team "Surrey" EvenlySpaced) (Team "VKC" EvenlySpaced)
    , Game (Team "Surrey" EvenlySpaced) (Team "St Albans" EvenlySpaced)
    , Game (Team "Surrey" EvenlySpaced) (Team "East End" EvenlySpaced)
    , Game (Team "Surrey" EvenlySpaced) (Team "Castle" EvenlySpaced)
    , Game (Team "VKC" EvenlySpaced) (Team "St Albans" EvenlySpaced)
    , Game (Team "VKC" EvenlySpaced) (Team "East End" EvenlySpaced)
    , Game (Team "VKC" EvenlySpaced) (Team "Castle" EvenlySpaced)
    , Game (Team "St Albans" EvenlySpaced) (Team "East End" EvenlySpaced)
    , Game (Team "St Albans" EvenlySpaced) (Team "Castle" EvenlySpaced)
    , Game (Team "East End" EvenlySpaced) (Team "Castle" EvenlySpaced)
    ]


init : Json.Encode.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init savedModelJson url key =
    let
        vanillaModelWithKey : Model
        vanillaModelWithKey =
            vanillaModel key

        initialModel : Model
        initialModel =
            Json.Decode.decodeValue (decodeModel vanillaModelWithKey) savedModelJson
                |> Result.withDefault vanillaModelWithKey
    in
    update (UrlChanged url) initialModel



---- UPDATE ----


type Msg
    = -- define games
      ShowGames
    | ShowAddGame
    | DeleteGame Game
    | AddExampleGames
    | PasteGames (List Game)
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
    | StartOptimiseGameOrder
    | OptimiseGameOrder
    | CopyAnalysedGames (List AnalysedGame)
      -- Tweak
    | DndMsg DnDList.Msg
      -- Routing
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Games
        ShowGames ->
            ( { model | uiState = GamesView }, Cmd.none )

        DeleteGame game ->
            { model | uiState = GamesView }
                |> setGames (List.Extra.remove game model.games)
                |> modelAndSaveToLocalStorage

        ShowAddGame ->
            ( { model | uiState = AddGameView, homeTeamToAdd = vanillaTeam, awayTeamToAdd = vanillaTeam }, Cmd.none )

        AddExampleGames ->
            setGames exampleGames model
                |> modelAndSaveToLocalStorage

        -- Add game
        SetHomeTeamNameToAdd teamName ->
            ( { model | homeTeamToAdd = List.Extra.find (\t -> t.name == teamName) model.teams |> Maybe.withDefault (Team teamName NoPreference) }, Cmd.none )

        SetAwayTeamNameToAdd teamName ->
            ( { model | awayTeamToAdd = List.Extra.find (\t -> t.name == teamName) model.teams |> Maybe.withDefault (Team teamName NoPreference) }, Cmd.none )

        AddGame ->
            { model | uiState = GamesView }
                |> setGames (Game model.homeTeamToAdd model.awayTeamToAdd :: model.games)
                |> modelAndSaveToLocalStorage

        -- Edit game
        ShowEditGame game ->
            ( { model | uiState = EditGameView, gameToEdit = game, homeTeamNameToEdit = game.homeTeam.name, awayTeamNameToEdit = game.awayTeam.name }, Cmd.none )

        SetEditedHomeTeam homeTeamNameToEdit ->
            ( { model | homeTeamNameToEdit = homeTeamNameToEdit }, Cmd.none )

        SetEditedAwayTeam awayTeamNameToEdit ->
            ( { model | awayTeamNameToEdit = awayTeamNameToEdit }, Cmd.none )

        EditGame ->
            let
                gameToEdit : Game
                gameToEdit =
                    model.gameToEdit

                newHomeTeam : Team
                newHomeTeam =
                    List.Extra.find (\t -> t.name == model.homeTeamNameToEdit) model.teams
                        |> Maybe.withDefault { vanillaTeam | name = model.homeTeamNameToEdit }

                newAwayTeam : Team
                newAwayTeam =
                    List.Extra.find (\t -> t.name == model.awayTeamNameToEdit) model.teams
                        |> Maybe.withDefault { vanillaTeam | name = model.awayTeamNameToEdit }

                newGames : List Game
                newGames =
                    List.Extra.setIf
                        ((==) gameToEdit)
                        { gameToEdit | homeTeam = newHomeTeam, awayTeam = newAwayTeam }
                        model.games
            in
            { model | uiState = GamesView }
                |> setGames newGames
                |> modelAndSaveToLocalStorage

        -- Teams
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

                editedGames : List Game
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
            { model | teams = editedTeams }
                |> setGames editedGames
                |> modelAndSaveToLocalStorage

        StartOptimiseGameOrder ->
            ( { model | optimising = True }, Process.sleep 1 |> Task.perform (always OptimiseGameOrder) )

        OptimiseGameOrder ->
            let
                games : List Game
                games =
                    Maybe.map (\justGameOrderMetrics -> List.map .game justGameOrderMetrics.analysedGames) model.gameOrderMetrics
                        |> Maybe.withDefault (model.games |> List.sortBy initialSortOrderForGame)

                gameOrderMetrics : GameOrderMetrics
                gameOrderMetrics =
                    optimiseAllPermutations games model.gameOrderMetrics
            in
            if Just gameOrderMetrics == model.gameOrderMetrics then
                -- no change from the last attempt, so finished the optimisation
                { model
                    | previousGameOrderMetrics = model.gameOrderMetrics
                    , gameOrderMetrics = Just gameOrderMetrics
                    , tweakedGameOrderMetrics = Just gameOrderMetrics
                    , optimising = False
                }
                    |> modelAndSaveToLocalStorage

            else
                -- we found a better optimisation, so try again for an even better one
                ( { model
                    | previousGameOrderMetrics = model.gameOrderMetrics
                    , gameOrderMetrics = Just gameOrderMetrics
                    , tweakedGameOrderMetrics = Just gameOrderMetrics
                  }
                , Process.sleep 1 |> Task.perform (always OptimiseGameOrder)
                )

        -- Copy / Paste
        CopyAnalysedGames analysedGames ->
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
                setGames games model
                    |> modelAndSaveToLocalStorage

            else
                ( model, Cmd.none )

        -- Tweak
        DndMsg dndMsg ->
            let
                ( dnd, updatedTweakedGames ) =
                    system.update dndMsg model.dnd (tweakedGamesWithDefault model)

                newModel : Model
                newModel =
                    { model
                        | dnd = dnd
                        , tweakedGameOrderMetrics = Just (calculateGameOrderMetrics (List.map .game updatedTweakedGames))
                    }
            in
            ( newModel
            , Cmd.batch [ system.commands dnd, encodeModel newModel |> Json.Encode.encode 2 |> saveToLocalStorage ]
            )

        -- Routing
        UrlChanged url ->
            if url.fragment == Just "/teams" && not (List.isEmpty model.games) then
                ( { model | uiState = TeamsView }, Cmd.none )

            else if url.fragment == Just "/options-explanation" && not (List.isEmpty model.games) then
                ( { model | uiState = TeamOptionsExplanationView }, Cmd.none )

            else if url.fragment == Just "/optimise" && not (List.isEmpty model.games) then
                ( { model | uiState = OptimiseView }, Cmd.none )

            else if url.fragment == Just "/tweak" && not (model.tweakedGameOrderMetrics == Nothing) then
                ( { model | uiState = TweakView }, Cmd.none )

            else
                ( { model | uiState = GamesView }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )


modelAndSaveToLocalStorage : Model -> ( Model, Cmd msg )
modelAndSaveToLocalStorage model =
    ( model, encodeModel model |> Json.Encode.encode 2 |> saveToLocalStorage )


tweakedGamesWithDefault : Model -> List AnalysedGame
tweakedGamesWithDefault model =
    Maybe.map .analysedGames model.tweakedGameOrderMetrics |> Maybe.withDefault []


clearOptimisationResults : Model -> Model
clearOptimisationResults model =
    { model | previousGameOrderMetrics = Nothing, gameOrderMetrics = Nothing }


setGames : List Game -> Model -> Model
setGames games model =
    let
        oldTeams : List Team
        oldTeams =
            List.filter (\team -> List.any (\game -> playing team game) games) model.teams

        newTeams : List Team
        newTeams =
            List.concatMap (\game -> [ game.homeTeam, game.awayTeam ]) games
                |> List.Extra.unique
                |> List.filter (\newTeam -> not (List.any (\oldTeam -> oldTeam.name == newTeam.name) oldTeams))
    in
    { model | games = games, teams = oldTeams ++ newTeams }
        |> clearOptimisationResults



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Tournament organiser"
    , body =
        [ div
            [ class "body stack stackSplitAfter2" ]
            [ header
                []
                [ h1 [] [ text "Tournament Organiser" ]
                , h2 [] [ text "Optimise Order of Games" ]
                , nav
                    [ class "center" ]
                    -- sort out the spans with some flex or something
                    [ if model.uiState == GamesView then
                        span [] [ text "Games" ]

                      else
                        a [ href "/" ] [ text "Games" ]
                    , if List.isEmpty model.games || model.uiState == TeamsView then
                        span [] [ text "Teams" ]

                      else
                        a [ href "/#/teams" ] [ text "Teams" ]
                    , if List.isEmpty model.games || model.uiState == OptimiseView then
                        span [] [ text "Optimise" ]

                      else
                        a [ href "/#/optimise" ] [ text "Optimise" ]
                    , if model.gameOrderMetrics == Nothing || model.uiState == TweakView then
                        span [] [ text "Tweak" ]

                      else
                        a [ href "/#/tweak" ] [ text "Tweak" ]
                    ]
                ]
            , main_
                [ class "center stack"
                , class (uiStateToString model.uiState)
                ]
                (stateView model)
            , footer
                []
                [ p
                    []
                    [ span
                        []
                        [ text "This page, with caching, was "
                        , strong
                            []
                            [ text "just "
                            , span
                                [ id "transferSizeKb" ]
                                []
                            , text "Kb"
                            ]
                        , text " (internet average ~3000kb) and loaded in "
                        , strong
                            []
                            [ text "just "
                            , span
                                [ id "loadTimeS" ]
                                []
                            , text " seconds"
                            ]
                        , text ". Very approximately, this equates to "
                        , strong
                            []
                            [ text "just "
                            , span
                                [ id "co2g" ]
                                []
                            ]
                        , text " grams of CO"
                        , sup
                            []
                            [ text "2" ]
                        , text " equivalent"
                        , text ". "
                        , a
                            [ href "http://green-pages.earth/page-statistics" ]
                            [ text "Read more ..." ]
                        ]
                    ]
                ]
            ]
        ]
    }


uiStateToString : UiState -> String
uiStateToString uiState =
    case uiState of
        GamesView ->
            "gamesView"

        AddGameView ->
            "addGameView"

        EditGameView ->
            "editGameView"

        TeamsView ->
            "teamsView"

        TeamOptionsExplanationView ->
            "teamOptionsExplanationView"

        OptimiseView ->
            "optimiseView"

        TweakView ->
            "tweakView"


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

        TeamOptionsExplanationView ->
            teamOptionsExplanationView

        OptimiseView ->
            optimiseView model

        TweakView ->
            tweakView model


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
        , deleteListItemButton (DeleteGame game)
        ]


addGameView : Model -> List (Html Msg)
addGameView model =
    [ teamsDataList model
    , div
        [ class "teamForm" ]
        [ input
            [ onInput SetHomeTeamNameToAdd
            , list "teamsDataList"
            , placeholder "Enter team ..."
            ]
            []
        , span
            []
            [ text "-" ]
        , input
            [ onInput SetAwayTeamNameToAdd
            , list "teamsDataList"
            , placeholder "Enter team ..."
            ]
            []
        ]
    , button
        [ disabled (model.homeTeamToAdd.name == "" || model.awayTeamToAdd.name == "" || model.homeTeamToAdd.name == model.awayTeamToAdd.name)
        , onClick AddGame
        , class "primary center"
        ]
        [ text "Add Game" ]
    , button
        [ onClick ShowGames
        , class "secondary center"
        ]
        [ text "Cancel" ]
    ]


editGameView : Model -> List (Html Msg)
editGameView model =
    [ teamsDataList model
    , div
        [ class "teamForm" ]
        [ input
            [ onInput SetEditedHomeTeam
            , list "teamsDataList"
            , placeholder "Enter team ..."
            , value model.homeTeamNameToEdit
            ]
            []
        , span
            []
            [ text "-" ]
        , input
            [ onInput SetEditedAwayTeam
            , list "teamsDataList"
            , placeholder "Enter team ..."
            , value model.awayTeamNameToEdit
            ]
            []
        ]
    , button
        [ disabled (model.homeTeamNameToEdit == "" || model.awayTeamNameToEdit == "" || model.homeTeamNameToEdit == model.awayTeamNameToEdit)
        , onClick EditGame
        , class "primary center"
        ]
        [ text "Edit Game" ]
    , button
        [ onClick ShowGames
        , class "secondary center"
        ]
        [ text "Cancel" ]
    ]


teamsDataList : Model -> Html Msg
teamsDataList model =
    datalist
        [ id "teamsDataList" ]
        (List.map (\team -> option [ value team.name ] []) model.teams)


teamsView : Model -> List (Html Msg)
teamsView model =
    [ p
        []
        [ text "Choose the team preferences. The optimiser will always try to make sure no teams play back to back games, "
        , a [ href "/#/options-explanation" ] [ text "more details here." ]
        ]
    , ul
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
        [ onInput (\s -> EditTournamentPreference team (stringToTournamentPreference s |> Maybe.withDefault NoPreference))
        , title "Choose team preference"
        ]
        [ tournamentPreferenceOption team NoPreference
        , tournamentPreferenceOption team StartLate
        , tournamentPreferenceOption team FinishEarly
        , tournamentPreferenceOption team EvenlySpaced
        ]


tournamentPreferenceOption : Team -> TournamentPreference -> Html Msg
tournamentPreferenceOption team tournamentPreference =
    option
        [ selected (team.tournamentPreference == tournamentPreference) ]
        [ text <| tournamentPreferenceToString tournamentPreference ]


teamOptionsExplanationView : List (Html Msg)
teamOptionsExplanationView =
    [ p [] [ text "The optimiser will always try and make sure no team plays back to back games. After that, it will optimise for the team prefences." ]
    , h3 [] [ text "Evenly Spaced" ]
    , p [] [ text "Evenly Spaced tries to ensure that the team always gets at least two games rest between games, and that the rest between games is consistent. For example, it is annoying to have two games close together and then have a long wait until the next one. This is the default (and usually best) option." ]
    , h3 [] [ text "Start Late" ]
    , p [] [ text "Start Late tries to ensure that the first game for a team is as late as possible. This option is harder to accomodate, and can de-optimise other teams, so it should be used sparingly (or not at all). Because it is hard to accomodate, it has a lower weight than other preferences." ]
    , h3 [] [ text "Finish Early" ]
    , p [] [ text "Finish Early tries to ensure that the last game for a team is as early as possible. For example, some teams with children need to get back as early as possible and get to bed. This option is harder to accomodate, and can de-optimise other teams, so it should be used sparingly (or not at all). Because it is hard to accomodate, it has a lower weight than other preferences." ]
    , h3 [] [ text "No preference" ]
    , p [] [ text "No preference will ignore the team when optimising (apart from making sure that they don't play back to back games)." ]
    ]


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
            model.gameOrderMetrics == Nothing || model.optimising

        analysedGames : List AnalysedGame
        analysedGames =
            Maybe.map .analysedGames model.gameOrderMetrics |> Maybe.withDefault []
    in
    button
        [ onClick StartOptimiseGameOrder
        , class "primary center"
        , disabled optimiseDisabled
        ]
        [ text
            (if model.optimising then
                "Optimising ..."

             else
                "Optimise"
            )
        ]
        :: button
            [ onClick (CopyAnalysedGames analysedGames)
            , class "secondary center"
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
        [ p [] [ text "Showing the best game order I found so far." ]
        , preferenceExplanation gameOrderMetrics
        , analysedTeamsExplanation gameOrderMetrics
        ]

    else
        [ p [] [ text "Showing the best game order I could find." ]
        , preferenceExplanation gameOrderMetrics
        , analysedTeamsExplanation gameOrderMetrics
        ]


analysedTeamsExplanation : GameOrderMetrics -> Html Msg
analysedTeamsExplanation gameOrderMetrics =
    let
        teamScores : String
        teamScores =
            List.map
                (\analysedTeam -> analysedTeam.team.name ++ " " ++ percentage analysedTeam.tournamentPreferenceScore ++ "%")
                gameOrderMetrics.analysedTeams
                |> String.join ", "
    in
    p
        []
        [ text teamScores ]


percentage : Float -> String
percentage zeroToOne =
    zeroToOne
        * 100
        |> floor
        |> String.fromInt


preferenceExplanation : GameOrderMetrics -> Html Msg
preferenceExplanation gameOrderMetrics =
    let
        lowestTournamentPreferenceScore : String
        lowestTournamentPreferenceScore =
            percentage gameOrderMetrics.lowestTournamentPreferenceScore

        highestTournamentPreferenceScore : String
        highestTournamentPreferenceScore =
            percentage gameOrderMetrics.highestTournamentPreferenceScore

        explanation : String
        explanation =
            if lowestTournamentPreferenceScore == highestTournamentPreferenceScore then
                "The team preferences are accommodated with "
                    ++ lowestTournamentPreferenceScore
                    ++ "% success"

            else
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
        (List.map analysedGameListItem analysedGames)


analysedGameListItem : AnalysedGame -> Html Msg
analysedGameListItem game =
    li
        [ class "game" ]
        (analysedGameView game)


analysedGameView : AnalysedGame -> List (Html Msg)
analysedGameView game =
    [ span
        [ class "homeTeam"
        , consecutiveGameClass game.homeTeamPlayingConsecutively
        ]
        [ text game.game.homeTeam.name ]
    , span
        []
        [ text "\u{00A0}-\u{00A0}" ]
    , span
        [ class "awayTeam"
        , consecutiveGameClass game.awayTeamPlayingConsecutively
        ]
        [ text game.game.awayTeam.name ]
    ]


tweakView : Model -> List (Html Msg)
tweakView model =
    let
        tweakedGames : List AnalysedGame
        tweakedGames =
            tweakedGamesWithDefault model
    in
    [ button
        [ onClick (CopyAnalysedGames tweakedGames)
        , class "primary center"
        ]
        [ text "Copy to clipboard" ]
    , p
        []
        [ text "Drag the games to reorder manually" ]

    -- it would be good to enforce that tweakedGameOrderMetrics exists using the type system,
    -- by putting it on the TweakView custom type, but fiddly to achieve with the Dnd package,
    -- this is easier and less code.
    , Maybe.map preferenceExplanation model.tweakedGameOrderMetrics |> Maybe.withDefault (text "")
    , div
        [ class "stack stack-small drag-drop" ]
        (List.indexedMap (tweakedGameView model.dnd) tweakedGames)
    , ghostView model.dnd tweakedGames
    ]


tweakedGameView : DnDList.Model -> Int -> AnalysedGame -> Html.Html Msg
tweakedGameView dnd index game =
    let
        gameId : String
        gameId =
            "tweaked-game-" ++ fromInt game.id
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                div
                    (id gameId
                        :: Html.Attributes.style "touch-action" "none"
                        :: system.dropEvents index gameId
                    )
                    [ span
                        [ class "game" ]
                        (analysedGameView game)
                    ]

            else
                div
                    [ id gameId, class "drag-underlay", Html.Attributes.style "touch-action" "none" ]
                    [ span
                        [ class "game" ]
                        (analysedGameView game)
                    ]

        Nothing ->
            div
                (id gameId :: Html.Attributes.style "touch-action" "none" :: system.dragEvents index gameId)
                [ span
                    [ class "game" ]
                    (analysedGameView game)
                ]


ghostView : DnDList.Model -> List AnalysedGame -> Html.Html Msg
ghostView dnd games =
    let
        maybeDragGame : Maybe AnalysedGame
        maybeDragGame =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> List.Extra.getAt dragIndex games)
    in
    case maybeDragGame of
        Just game ->
            div
                (class "drag-ghost" :: system.ghostStyles dnd)
                [ span
                    [ class "game" ]
                    (analysedGameView game)
                ]

        Nothing ->
            text ""


consecutiveGameClass : Bool -> Html.Attribute Msg
consecutiveGameClass playingConsecutiveley =
    if playingConsecutiveley then
        class "playingConsecutively"

    else
        class "atLeastOneGameToRecover"


deleteListItemButton : msg -> Html msg
deleteListItemButton onDelete =
    button
        [ onClick onDelete
        , title "Delete"
        ]
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
        [ onClick onEdit
        , title "Edit"
        ]
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


main : Program Json.Encode.Value Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
