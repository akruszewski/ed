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
                                                , look
                                                , readP_to_S
                                                , satisfy
                                                )
import           Event

toAction :: String -> Maybe EditorAction
toAction input = case readP_to_S parseAction input of
  []         -> Nothing
  [(res, _)] -> Just res
  _          -> Nothing

parseAction :: ReadP EditorAction
parseAction =
  parseRegexAction
    <|> parseActionWithArgument
    <|> parseAddressAction
    <|> parseSimpleAction

parseAddressAction :: ReadP EditorAction
parseAddressAction = do
  address <- parseLines
  action  <- satisfy isActionWithAddress
  return $ case action of
    'p' -> Print address
    'a' -> Append address
    'i' -> Insert address
    'd' -> Delete address

parseSimpleAction :: ReadP EditorAction
parseSimpleAction = do
  satisfy isActionWithoutArgumentAndAddress >>= \case
    '+' -> return NLine
    '-' -> return PLine
    '*' -> return Debug
    'q' -> return Quit
    'w' -> return $ SaveFile Nothing

parseActionWithArgument :: ReadP EditorAction
parseActionWithArgument = do
  action <- satisfy isActionWithArgument
  satisfy (== ' ')
  xs <- munch1 isPath
  return $ case action of
    'w' -> SaveFile (Just xs)

parseRegexAction :: ReadP EditorAction
parseRegexAction = do
  action <- satisfy isLineAddressWithArgs
  xs     <- look -- TODO: characters here should be consumed.
  return $ case action of
    '/' -> Search Forward $ T.pack xs
    '?' -> Search Backward $ T.pack xs

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
parseLine = satisfy isLineAddress >>= \case
  ',' -> return AllLines
  '%' -> return AllLines
  ';' -> return LinesFromCurrentToLast
  '.' -> return CurrentLine
  '$' -> return LastLine
  '-' -> return PreviousLine
  '^' -> return PreviousLine
  '+' -> return NextLine

parseLineAddress :: ReadP Address
parseLineAddress = many1 number >>= \x -> return $ Line $ read x

number :: ReadP Char
number = satisfy isNumber

isAlphaNum :: Char -> Bool
isAlphaNum x = isAlpha x || isNumber x || isComa x

-- TODO: a lot of characters are missing here, refactor.
isPath :: Char -> Bool
isPath x = isAlpha x || isNumber x || isComa x || x == '/' || x == '.'

isComa :: Char -> Bool
isComa c = c == ','

-- isAction :: Char -> Bool
-- isAction c = c `elem` ("acdeEfgGHhijklmnpPqQrsStuvVwWz=!" :: String)

isActionWithAddress :: Char -> Bool
isActionWithAddress c = c `elem` ("pad" :: String)

isActionWithArgument :: Char -> Bool
isActionWithArgument c = c `elem` ("w" :: String)

isActionWithoutArgumentAndAddress :: Char -> Bool
isActionWithoutArgumentAndAddress c = c `elem` ("+-*qw" :: String)

isLineAddress :: Char -> Bool
isLineAddress c = c `elem` (".$-^+,%;" :: String)

isLineAddressWithArgs :: Char -> Bool
isLineAddressWithArgs c = c `elem` ("/?'" :: String)
