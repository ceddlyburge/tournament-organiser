module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, p, span, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type UiState
    = TeamsView
    | AddTeamView
    | EditTeamView
    | GameOrderView


type alias Team =
    { name : String
    }


vanillaTeam : Team
vanillaTeam =
    Team ""


type alias Model =
    { uiState : UiState
    , teams : List Team

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
        ""
        vanillaTeam
        ""


init : ( Model, Cmd Msg )
init =
    ( vanillaModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | DeleteTeam Team
      -- add team
    | ShowAddTeam
    | SetTeamNameToAdd String
    | AddTeam
      -- edit team
    | ShowEditTeam Team
    | SetEditedTeamName String
    | EditTeam


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteTeam team ->
            ( { model | teams = List.filter (\t -> t /= team) model.teams }, Cmd.none )

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

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.main_
        []
        [ Html.header
            []
            [ h1 [] [ text "Tournament Organiser - Optimise Order of Games" ]
            , p [] [ text "Which teams are playing at this tournament?" ]
            ]
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
            []


addTeamView : List (Html Msg)
addTeamView =
    [ input
        [ onInput SetTeamNameToAdd ]
        []
    , button
        [ onClick AddTeam ]
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


teamsView : Model -> List (Html Msg)
teamsView model =
    button
        [ onClick ShowAddTeam ]
        [ text "Add Team" ]
        :: List.map teamView model.teams


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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
