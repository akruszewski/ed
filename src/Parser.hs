{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseAction
  , toAction
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( isNumber
                                                , isAlpha
                                                )
import qualified Data.Text                     as T
import           Text.ParserCombinators.ReadP   ( ReadP(..)
                                                , many1
                                                , munch1
                                                , readP_to_S
                                                , satisfy
                                                )
import           Event


-------------------------------------------------------------------------------
-- PARSER
-------------------------------------------------------------------------------
-- TODO:
--  - implement all comands from https://man.openbsd.org/ed.1

toAction :: String -> Maybe EditorAction
toAction input = case readP_to_S parseAction input of
  []         -> Nothing
  [(res, _)] -> Just res
  _          -> Nothing

parseAction :: ReadP EditorAction
parseAction =
  parseSimpleAction <|> parseActionWithArgument <|> parseAddressAction

parseAddressAction :: ReadP EditorAction
parseAddressAction = do
  address <- parseLines
  action  <- satisfy isActionWithAddress
  return $ case action of
    'p' -> Print address
    'a' -> Insert address
    'd' -> Delete address

parseSimpleAction :: ReadP EditorAction
parseSimpleAction = do
  satisfy isActionWithoutArgumentAndAddress >>= \case
    '+' -> return NLine
    '-' -> return PLine
    '*' -> return Debug
    'q' -> return Quit

parseActionWithArgument :: ReadP EditorAction
parseActionWithArgument = do
  action <- satisfy isActionWithArgument
  satisfy (== ' ')
  xs <- munch1 isAlphaNum
  return $ case action of
    'w' -> SaveFile (Just xs)

parseLines :: ReadP Address
parseLines =
  parseRangeLines <|> parseLineAddress <|> parseLine <|> return CurrentLine

parseRangeLines :: ReadP Address
parseRangeLines = do
  from <- many1 number
  satisfy (== ',')
  to <- many1 number
  return $ Lines (read from) (read to)

parseLine :: ReadP Address
parseLine = parseLineWithArguments <|> parseLineWithoudArguments

parseLineWithoudArguments :: ReadP Address
parseLineWithoudArguments = satisfy isLineAddress >>= \case
  ',' -> return AllLines
  '%' -> return AllLines
  ';' -> return LinesFromCurrentToLast
  '.' -> return CurrentLine
  '$' -> return LastLine
  '-' -> return PreviousLine
  '^' -> return PreviousLine
  '+' -> return NextLine

parseLineWithArguments :: ReadP Address
parseLineWithArguments = satisfy isLineAddressWithArgs >>= \case
  '/' -> do
    text <- munch1 isAlphaNum
    return $ NextRegexLine $ T.pack text
  '?' -> do
    text <- munch1 (\x -> isAlpha x || isNumber x || isComa x)
    return $ PrevRegexLine $ T.pack text
  '\'' -> do
    x <- munch1 isNumber
    return $ MarkedLine $ read x


parseLineAddress :: ReadP Address
parseLineAddress = many1 number >>= \x -> return $ Line $ read x

number :: ReadP Char
number = satisfy isNumber

-- coma :: ReadP Char
-- coma = satisfy isComa
isAlphaNum :: Char -> Bool
isAlphaNum x = isAlpha x || isNumber x || isComa x

isComa :: Char -> Bool
isComa c = c == ','

-- isAction :: Char -> Bool
-- isAction c = c `elem` ("acdeEfgGHhijklmnpPqQrsStuvVwWz=!" :: String)

isActionWithAddress :: Char -> Bool
isActionWithAddress c = c `elem` ("pad" :: String)

isActionWithArgument :: Char -> Bool
isActionWithArgument c = c `elem` ("w" :: String)

isActionWithoutArgumentAndAddress :: Char -> Bool
isActionWithoutArgumentAndAddress c = c `elem` ("+-*q" :: String)

isLineAddress :: Char -> Bool
isLineAddress c = c `elem` (".$-^+,%;" :: String)

isLineAddressWithArgs :: Char -> Bool
isLineAddressWithArgs c = c `elem` ("/?'" :: String)
