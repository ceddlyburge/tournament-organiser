module GameParserTests exposing (all)

import Expect
import GameParser exposing (gameParser, gamesParser)
import Optimisation.GameOrderMetrics exposing (Game, Team, TournamentPreference(..))
import Parser
import Test exposing (Test, describe, test)


all : Test
all =
    describe "parser"
        [ test "One game" <|
            \_ ->
                Parser.run gameParser "ULU\tCastle"
                    |> Expect.equal (Ok (Game (Team "ULU" NoPreference) (Team "Castle" NoPreference)))
        , test "Two games" <|
            \_ ->
                Parser.run gamesParser "ULU\tCastle\nULU\tEast End"
                    |> Expect.equal
                        (Ok
                            [ Game (Team "ULU" NoPreference) (Team "Castle" NoPreference)
                            , Game (Team "ULU" NoPreference) (Team "East End" NoPreference)
                            ]
                        )
        , test "Two games crlf" <|
            \_ ->
                Parser.run gamesParser "ULU\tCastle\u{000D}\nULU\tEast End"
                    |> Expect.equal
                        (Ok
                            [ Game (Team "ULU" NoPreference) (Team "Castle" NoPreference)
                            , Game (Team "ULU" NoPreference) (Team "East End" NoPreference)
                            ]
                        )
        , test "Two games trailing lf" <|
            \_ ->
                Parser.run gamesParser "ULU\tCastle\nULU\tEast End\n"
                    |> Expect.equal
                        (Ok
                            [ Game (Team "ULU" NoPreference) (Team "Castle" NoPreference)
                            , Game (Team "ULU" NoPreference) (Team "East End" NoPreference)
                            ]
                        )
        ]
