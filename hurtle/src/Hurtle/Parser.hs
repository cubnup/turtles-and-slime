{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Hurtle.Parser (parseHogo) where

import Hurtle.Types


-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec ((<|>), MonadParsec(try, eof), sepEndBy, many)
import Text.Megaparsec.Char ( hspace, hspace1, space, string)
import Text.Megaparsec.Char.Lexer (decimal, skipLineComment, float)

-- first consume a lineEnd to handle comments/whitespace before the first program
-- then store the parsed program in out, separated and ended by comments/whitespace
-- sepEndBy works in that if it fails, it doesnt parse error, but rather it just stops parsing and returns nothing
-- because of this, we can check that eof is immediately after parseHogo
-- if it isn't this calls a parse error!
-- return out :: HogoProgram lifted in Parser
parseHogo :: Parser HogoProgram
parseHogo = do
    lineEnd
    out <- parseCode `sepEndBy` lineEnd
    eof
    pure out


parseCode :: Parser HogoCode
-- we fold across the commands, starting with goForward
parseCode = foldl (<|>) goForward commands 
    where
        --construct a list of the possible commands, minus goForward as we start with it
        commands = [
            goBackward, turnLeft, turnRight,
            repeat',
            penUp, penDown,
            goHome, clearScreen
            ]

        -- these abstract the idea of the certain genre of commands that take in a number after
        -- type is Parser Float/Int so we can use fmap or some other way to map it to Parser HogoCode as needed!!
        floatCode name = do
            string name
            hspace1
            try float <|> decimal
        intCode name = do
            string name
            hspace1
            decimal

        -- take in repeat number first, using intCode into num
        -- then consume a [
        -- then we parse the hogo program inside the []
        -- keep parsing until we reach ]
        -- since we are parsing as we go with parseHogo', nested repeats are handled recursively,
            -- i.e. parseHogo consumes nested ] instead of the string "]"
        -- then we lift everything in Repeat!
        repeat' :: Parser HogoCode
        repeat'      = do
            num <- intCode "repeat"
            hspace
            string "["
            -- can't use parseHogo because of eof in parseHogo
            let parseHogo' = lineEnd >> parseCode `sepEndBy` lineEnd
            subP <-  parseHogo'
            string "]"
            pure $ Repeat num subP
        -- all of these are :: Parser HogoCode
        -- fmap the HogoCode constructor(e.g GoForawrd) over the floatCode, so you get Parser Float -> Parser HogoCode
        goForward   =   GoForward  <$> floatCode "forward"
        goBackward  =   GoBackward <$> floatCode "back"
        turnLeft    =   TurnLeft   <$> floatCode "left"
        turnRight   =   TurnRight  <$> floatCode "right"
        --these are simple just check if the string is there and return the code
        goHome      = do
            string "home"
            pure GoHome
        penUp       = do
            string "penup"
            pure PenUp
        penDown     = do
            string "pendown"
            pure PenDown
        clearScreen = do
            string "clearscreen"
            pure ClearScreen


-- try skipping comments as much as possible, then consume whitespace/newlines
lineEnd :: Parser ()
lineEnd = try $ do
    many $ try skip
    space
    where skip = space >> skipLineComment ";"

