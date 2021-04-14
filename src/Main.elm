module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, p, section, span, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)
import List.Extra



---- MODEL ----


type UiState
    = TeamsView
    | AddTeamView
    | EditTeamView
    | GameOrderView
    | AddGameView


type alias Team =
    { name : String
    }


vanillaTeam : Team
vanillaTeam =
    Team ""


type alias Game =
    { homeTeam : Team
    , awayTeam : Team
    }


type alias Model =
    { uiState : UiState
    , teams : List Team
    , games : List Game

    -- add team
    , teamNameToAdd : String

    -- edit team
    , teamToEdit : Team
    , editedTeamName : String
    }


vanillaModel : Model
vanillaModel =
    Model
        TeamsView
        [ Team "Castle"
        , Team "Blackwater"
        , Team "ULU"
        ]
        []
        ""
        vanillaTeam
        ""


init : ( Model, Cmd Msg )
init =
    ( vanillaModel, Cmd.none )



---- UPDATE ----


type Msg
    = -- delete team
      DeleteTeam Team
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteTeam team ->
            ( { model | teams = List.Extra.remove team model.teams }, Cmd.none )

        -- Add team
        ShowAddTeam ->
            ( { model | uiState = AddTeamView }, Cmd.none )

        SetTeamNameToAdd teamName ->
            ( { model | teamNameToAdd = teamName }, Cmd.none )

        AddTeam ->
            ( { model | teams = Team model.teamNameToAdd :: model.teams, uiState = TeamsView }, Cmd.none )

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

        -- Add team
        ShowAddGame ->
            ( { model | uiState = AddGameView }, Cmd.none )


initializeGames : List Team -> List Game -> List Game
initializeGames teams existingGames =
    case existingGames of
        [] ->
            List.Extra.uniquePairs teams
                |> List.foldl (\( team1, team2 ) games -> Game team1 team2 :: Game team2 team1 :: games) []

        someGames ->
            someGames



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.main_
        []
        [ Html.header
            []
            [ h1 [] [ text "Tournament Organiser - Optimise Order of Games" ] ]
        , Html.section
            []
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


addTeamView : List (Html Msg)
addTeamView =
    [ p
        []
        [ text "Which teams are playing at this tournament?" ]
    , input
        [ onInput SetTeamNameToAdd ]
        []
    , button
        [ onClick AddTeam ]
        [ text "Add Team" ]
    ]


editTeamView : Model -> List (Html Msg)
editTeamView model =
    [ p
        []
        [ text "Which teams are playing at this tournament?" ]
    , input
        [ onInput SetEditedTeamName
        , Html.Attributes.value model.editedTeamName
        ]
        []
    , button
        [ onClick EditTeam ]
        [ text "Edit Team" ]
    ]


teamsView : Model -> List (Html Msg)
teamsView model =
    [ p
        []
        [ text "Which teams are playing at this tournament?" ]
    , section
        []
        [ button
            [ onClick ShowAddTeam ]
            [ text "Add Team" ]
        ]
    , section
        []
        [ button
            [ onClick ShowGameOrder ]
            [ text "Define Games" ]
        ]
    , section
        []
        (List.map teamView model.teams)
    ]


teamView : Team -> Html Msg
teamView aTeam =
    p
        []
        [ span
            [ onClick (ShowEditTeam aTeam) ]
            [ text aTeam.name ]
        , button
            [ onClick (DeleteTeam aTeam) ]
            [ text "Delete" ]
        ]


gameOrderView : Model -> List (Html Msg)
gameOrderView model =
    [ p
        []
        [ text "Which games are taking place at this tournament?" ]
    , section
        []
        [ button
            [ onClick ShowAddGame ]
            [ text "Add Game" ]
        ]
    , section
        []
        (List.map gameView model.games)
    ]


gameView : Game -> Html Msg
gameView game =
    p
        []
        [ span
            []
            [ text game.homeTeam.name ]
        , span
            []
            [ text " - " ]
        , span
            []
            [ text game.awayTeam.name ]
        , button
            [ onClick (DeleteGame game) ]
            [ text "Delete" ]
        ]


addGameView : Model -> List (Html Msg)
addGameView model =
    [ section
        []
        [ text "Which games are taking place at this tournament?" ]

    -- todo
    --, input
    --    [ onInput SetTeamNameToAdd ]
    --    []
    , button
        []
        -- todo [ onClick AddTeam ]
        [ text "Add Game" ]
    ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
