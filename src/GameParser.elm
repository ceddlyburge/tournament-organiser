module GameParser exposing (gameParser, gamesParser)

import Optimisation.GameOrderMetrics exposing (Game, Team, TournamentPreference(..))
import Parser exposing (Parser, (|=), (|.))


gamesParser : Parser (List Game)
gamesParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.chompWhile (\char -> char == '\u{000D}')
        , item = gameParser
        , trailing = Parser.Optional
        }


gameParser : Parser Game
gameParser =
    Parser.succeed Game
        |= teamParser
        |. Parser.symbol "\t"
        |= teamParser


teamParser : Parser Team
teamParser =
    Parser.chompWhile (\char -> Char.isAlphaNum char || char == ' ')
        |> Parser.getChompedString
        |> Parser.map (\name -> Team name NoPreference)
