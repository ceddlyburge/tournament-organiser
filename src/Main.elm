module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Team =
    { name : String
    }


type alias Model =
    { teams : List Team
    }


vanillaModel : Model
vanillaModel =
    Model
        [ Team "Castle"
        , Team "Blackwater"
        , Team "ULU"
        ]


init : ( Model, Cmd Msg )
init =
    ( vanillaModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | DeleteTeam Team


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteTeam team ->
            ( { model | teams = List.filter (\t -> t /= team) model.teams }, Cmd.none )

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
            (teamsView model)
        ]


teamsView : Model -> List (Html Msg)
teamsView model =
    List.map teamView model.teams


teamView : Team -> Html Msg
teamView aTeam =
    p
        []
        [ text aTeam.name
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
