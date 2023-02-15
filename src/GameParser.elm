module GameParser exposing (..)

import Optimisation.GameOrderMetrics exposing (Game, Team, TournamentPreference(..))
import Parser exposing (..)


gamesParser : Parser (List Game)
gamesParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = chompWhile (\char -> char == '\u{000D}')
        , item = gameParser
        , trailing = Optional
        }


gameParser : Parser Game
gameParser =
    succeed Game
        |= teamParser
        |. symbol "\t"
        |= teamParser


teamParser : Parser Team
teamParser =
    chompWhile (\char -> Char.isAlphaNum char || char == ' ')
        |> getChompedString
        |> Parser.map (\name -> Team name NoPreference)
