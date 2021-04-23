module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, option, p, section, select, span, text)
import Html.Attributes exposing (class, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode as Encode
import List.Extra
import RemoteData exposing (WebData)



---- MODEL ----
-- type to represent the result of the optimization api


type alias Optimisation =
    { optimisedGameOrder : OptimisedGameOrder
    , pragmatisationLevel : Int
    , optimisationMessage : String
    , optimisedGameOrderAvailable : Bool
    }


type alias OptimisedGameOrder =
    { gameOrder : List OptimisedGame
    , occurencesOfTeamsPlayingConsecutiveGames : Int
    , maxPlayingInConsecutiveGames : Int
    , gamesNotPlayedBetweenFirstAndLast : Int
    }


type alias OptimisedTeam =
    { name : String
    }


type alias OptimisedGame =
    { homeTeamPlayingConsecutively : Bool
    , awayTeamPlayingConsecutively : Bool
    , homeTeam : OptimisedTeam
    , awayTeam : OptimisedTeam
    }


decodeOptimisation : Json.Decode.Decoder Optimisation
decodeOptimisation =
    Json.Decode.succeed Optimisation
        |> Json.Decode.Pipeline.required "optimisedGameOrder" decodeOptimisedGames
        |> Json.Decode.Pipeline.required "pragmatisationLevel" Json.Decode.int
        |> Json.Decode.Pipeline.required "optimisationMessage" Json.Decode.string
        |> Json.Decode.Pipeline.required "optimisedGameOrderAvailable" Json.Decode.bool


decodeOptimisedGames : Json.Decode.Decoder OptimisedGameOrder
decodeOptimisedGames =
    Json.Decode.succeed OptimisedGameOrder
        |> Json.Decode.Pipeline.required "gameOrder" (Json.Decode.list decodeOptimisedGame)
        |> Json.Decode.Pipeline.required "occurencesOfTeamsPlayingConsecutiveGames" Json.Decode.int
        |> Json.Decode.Pipeline.required "maxPlayingInConsecutiveGames" Json.Decode.int
        |> Json.Decode.Pipeline.required "gamesNotPlayedBetweenFirstAndLast" Json.Decode.int


decodeOptimisedGame : Json.Decode.Decoder OptimisedGame
decodeOptimisedGame =
    Json.Decode.succeed OptimisedGame
        |> Json.Decode.Pipeline.required "homeTeamPlayingConsecutively" Json.Decode.bool
        |> Json.Decode.Pipeline.required "awayTeamPlayingConsecutively" Json.Decode.bool
        |> Json.Decode.Pipeline.required "homeTeam" decodeOptimisedTeam
        |> Json.Decode.Pipeline.required "awayTeam" decodeOptimisedTeam


decodeOptimisedTeam : Json.Decode.Decoder OptimisedTeam
decodeOptimisedTeam =
    Json.Decode.succeed OptimisedTeam
        |> Json.Decode.Pipeline.required "name" Json.Decode.string



-- rest of model


type UiState
    = TeamsView
    | AddTeamView
    | EditTeamView
    | GameOrderView
    | AddGameView
    | EditGameView


type alias Team =
    { name : String
    }


vanillaTeam : Team
vanillaTeam =
    Team ""



-- maybe turn this in to a opaque type / parse don't validate thing later, to hide the order, and make sure that there are no two games with the same order number


type alias Game =
    { homeTeam : Team
    , awayTeam : Team
    , homeTeamPlayingConsecutively : Maybe Bool
    , awayTeamPlayingConsecutively : Maybe Bool
    , order : Int
    }


vanillaGame : Game
vanillaGame =
    Game
        vanillaTeam
        vanillaTeam
        Nothing
        Nothing
        0


encodeGame : Game -> Encode.Value
encodeGame game =
    Encode.object
        [ ( "HomeTeam", Encode.string game.homeTeam.name )
        , ( "AwayTeam", Encode.string game.awayTeam.name )
        ]


encodeGames : List Game -> Encode.Value
encodeGames games =
    Encode.list encodeGame games


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

    -- call to optimize api
    , optimisation : WebData Optimisation
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
        vanillaTeam
        vanillaTeam
        vanillaGame
        vanillaTeam
        vanillaTeam
        RemoteData.NotAsked


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
    | OptimiseGameOrder
    | OptimiseGameOrderResponse (WebData Optimisation)


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
                        | games = Game model.homeTeamToAdd model.awayTeamToAdd Nothing Nothing (List.length model.games) :: model.games
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

        OptimiseGameOrder ->
            ( { model | optimisation = RemoteData.Loading }
            , Http.post
                { url = "http://localhost:7071/api/OptimalGameOrder"
                , body = encodeGames model.games |> Http.jsonBody
                , expect = Http.expectJson (RemoteData.fromResult >> OptimiseGameOrderResponse) decodeOptimisation
                }
            )

        OptimiseGameOrderResponse response ->
            case response of
                RemoteData.Success optimisation ->
                    ( { model
                        | optimisation = response

                        -- if we add other things to Team (such as whether they want to leave early)
                        -- we will have to match up the OptimisedTeam's with the Team's
                        , games = List.indexedMap (\index game -> Game (Team game.homeTeam.name) (Team game.awayTeam.name) (Just game.homeTeamPlayingConsecutively) (Just game.awayTeamPlayingConsecutively) index) optimisation.optimisedGameOrder.gameOrder
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | optimisation = response }
                    , Cmd.none
                    )


initializeGames : List Team -> List Game -> List Game
initializeGames teams existingGames =
    case existingGames of
        [] ->
            List.Extra.uniquePairs teams
                |> List.foldl (\( team1, team2 ) gameTuples -> ( team1, team2 ) :: ( team2, team1 ) :: gameTuples) []
                |> List.indexedMap (\index ( homeTeam, awayTeam ) -> Game homeTeam awayTeam Nothing Nothing index)

        someGames ->
            someGames


clearOptimisationResults : Model -> Model
clearOptimisationResults model =
    { model
        | optimisation = RemoteData.NotAsked
        , games = List.map (\game -> Game game.homeTeam game.awayTeam Nothing Nothing game.order) model.games
    }



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

        EditGameView ->
            editGameView model


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
    case model.optimisation of
        RemoteData.Success optimisation ->
            p [] [ text optimisation.optimisationMessage ] :: gameOrderView2 model

        RemoteData.NotAsked ->
            gameOrderView2 model

        RemoteData.Loading ->
            [ loading ]

        RemoteData.Failure error ->
            p [] [ text (httpErrorMessage error) ] :: gameOrderView2 model


gameOrderView2 : Model -> List (Html Msg)
gameOrderView2 model =
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
        [ button
            [ onClick OptimiseGameOrder ]
            [ text "Optimise Game Order" ]
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
            (consecutiveGameClass game.homeTeamPlayingConsecutively)
            [ text game.homeTeam.name ]
        , span
            []
            [ text " - " ]
        , span
            (consecutiveGameClass game.awayTeamPlayingConsecutively)
            [ text game.awayTeam.name ]
        , button
            [ onClick (ShowEditGame game) ]
            [ text "Edit" ]
        , button
            [ onClick (DeleteGame game) ]
            [ text "Delete" ]
        ]


consecutiveGameClass : Maybe Bool -> List (Html.Attribute Msg)
consecutiveGameClass maybePlayingConsecutiveley =
    case maybePlayingConsecutiveley of
        Just playingConsecutiveley ->
            if playingConsecutiveley then
                [ class "playingConsecutively" ]

            else
                [ class "atLeastOneGameToRecover" ]

        Nothing ->
            []


addGameView : Model -> List (Html Msg)
addGameView model =
    [ section
        []
        [ text "Which games are taking place at this tournament?" ]
    , select
        [ onInput SetHomeTeamNameToAdd ]
        (option [] [ text "Please select team" ] :: List.map (\t -> option [] [ text t.name ]) model.teams)
    , span
        []
        [ text " - " ]
    , select
        [ onInput SetAwayTeamNameToAdd ]
        (option [] [ text "Please select team" ] :: List.map (\t -> option [] [ text t.name ]) model.teams)
    , button
        [ onClick AddGame ]
        [ text "Add Game" ]
    ]


editGameView : Model -> List (Html Msg)
editGameView model =
    [ section
        []
        [ text "Which games are taking place at this tournament?" ]
    , select
        [ onInput SetEditedHomeTeam ]
        (option [] [ text "Please select team" ] :: List.map (\t -> option [ Html.Attributes.selected (t == model.editedHomeTeam) ] [ text t.name ]) model.teams)
    , span
        []
        [ text " - " ]
    , select
        [ onInput SetEditedAwayTeam
        , Html.Attributes.value model.editedAwayTeam.name
        ]
        (option [] [ text "Please select team" ] :: List.map (\t -> option [ Html.Attributes.selected (t == model.editedAwayTeam) ] [ text t.name ]) model.teams)
    , button
        [ onClick EditGame ]
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


httpErrorMessage : Http.Error -> String
httpErrorMessage error =
    case error of
        Http.Timeout ->
            "Timeout, maybe there is no connection to the internet, or the optimisation server is down"

        Http.NetworkError ->
            "Network error, maybe there is no connection to the internet, or the optimisation server is down"

        Http.BadBody message ->
            "Unexpected Body. Hmmm, there is probably a problem in my configuration, please create an issue. Error returned: " ++ message

        Http.BadStatus statusCode ->
            "Bad Response. Hmmm, there is probably a problem in my configuration, or there is a problem with the optimisation server. Please create an issue. Status returned: " ++ String.fromInt statusCode

        Http.BadUrl message ->
            "Bad Url. Hmmm, there is probably a problem in my configuration, please create an issue. Error returned: " ++ message



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
