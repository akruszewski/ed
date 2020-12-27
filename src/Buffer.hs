{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Buffer
  ( Buffer(..)
  , SearchPhrase(..)
  , emptyBuffer
  , extendBuffer
  , initialBuffer
  , insertBuffer
  , getBufferLine
  , getBufferLines
  , incCurrentLine
  , decCurrentLine
  , deleteLineFromBuffer
  , deleteLinesFromBuffer
  , updateNextMatch
  , updateBufferNextMatch
  , getBufferLastMatch
  , emptySearchPhrase
  )
where

import           Data.Vector                    ( (!?)
                                                , (//)
                                                )
import qualified Data.Text                     as T
import qualified Data.Vector                   as Vector

import           Direction


data Buffer = Buffer
  { bufferFileName   :: String
  , cursorPosition   :: Int
  , markedPosition   :: Int
  , lastSearchPhrase :: SearchPhrase
  , bufferContent    :: Vector.Vector T.Text
  }
  deriving (Eq, Show)

data SearchPhrase = SearchPhrase
  { phrase        :: T.Text
  , nextMatch     :: Int
  , cachedMatches :: Vector.Vector (Int, T.Text, [T.Text])
  }
  deriving (Eq, Show)

emptyBuffer :: Buffer
emptyBuffer = Buffer "" 0 0 emptySearchPhrase Vector.empty

initialBuffer :: String -> T.Text -> Buffer
initialBuffer name body = Buffer name cp 0 emptySearchPhrase content
 where
  content = Vector.fromList (T.lines body)
  cp      = Vector.length content - 1

insertBuffer :: T.Text -> Buffer -> Buffer
insertBuffer newContent buf = buf { bufferContent  = newBufferContent
                                  , cursorPosition = cp
                                  }
 where

  newBufferContent :: Vector.Vector T.Text
  newBufferContent
    | cp >= prevContentLength = Vector.snoc prevContent newContent
    | cp < prevContentLength  = bufferContent buf // [(cp, newContent)]

  cp                = cursorPosition buf + 1
  prevContentLength = Vector.length prevContent
  prevContent       = bufferContent buf

extendBuffer :: [T.Text] -> Int -> Buffer -> Maybe Buffer
extendBuffer newContent position buf =
  case newBufferContent newContent position (bufferContent buf) of
    Just content -> Just buf
      { bufferContent  = content
      , cursorPosition = position + length newContent - 1
      }
    Nothing -> Nothing

newBufferContent
  :: [T.Text] -> Int -> Vector.Vector T.Text -> Maybe (Vector.Vector T.Text)
newBufferContent newContent position prevContent
  | position < prevContentLength
  = let (before, after) = Vector.splitAt position prevContent
    in  Just $ Vector.concat [before, newContentVector, Vector.tail after]
  | position == prevContentLength
  = Just $ Vector.concat [prevContent, newContentVector]
  | otherwise
  = Nothing

 where
  newContentVector  = Vector.fromList newContent
  prevContentLength = Vector.length prevContent

getBufferLine :: Buffer -> Int -> Maybe T.Text
getBufferLine = fmap (!?) bufferContent

-- TODO: Should return Either T.Text (Vector.Vector T.Text)
getBufferLines :: Buffer -> Int -> Int -> Maybe (Vector.Vector T.Text)
getBufferLines buf from to
  | Vector.length bc > to && from > 0 && to > 0 = Just
  $ Vector.slice (from - 1) to bc
  | otherwise = Nothing
  where bc = bufferContent buf

incCurrentLine :: Buffer -> Buffer
incCurrentLine buf = buf { cursorPosition = cursorPosition buf + 1 }

decCurrentLine :: Buffer -> Buffer
decCurrentLine buf = buf { cursorPosition = cursorPosition buf - 1 }

deleteLineFromBuffer :: Buffer -> Int -> Buffer
deleteLineFromBuffer buf idx = buf
  { bufferContent = Vector.concat [prev, Vector.tail next]
  }
  where (prev, next) = Vector.splitAt idx (bufferContent buf)

deleteLinesFromBuffer :: Buffer -> Int -> Int -> Buffer
deleteLinesFromBuffer buf from to = buf
  { bufferContent = Vector.concat [prev, next]
  }
 where
  (prev, xs) = Vector.splitAt from (bufferContent buf)
  next       = Vector.drop (to - from) xs

emptySearchPhrase :: SearchPhrase
emptySearchPhrase = SearchPhrase "" 0 Vector.empty

updateNextMatch :: Direction -> SearchPhrase -> SearchPhrase
updateNextMatch dir sp
  | dir == Forward = sp { nextMatch = if nx >= matchesLen then 0 else nx + 1 }
  | dir == Backward = sp
    { nextMatch = if nx == 0 then matchesLen - 1 else nx - 1
    }
 where
  nx         = nextMatch sp
  matches    = cachedMatches sp
  matchesLen = Vector.length matches

updateBufferNextMatch :: Direction -> Buffer -> Buffer
updateBufferNextMatch dir buf = buf
  { lastSearchPhrase = updateNextMatch dir lsp
  , cursorPosition   = nextMatch lsp
  }
  where lsp = lastSearchPhrase buf

getBufferLastMatch :: Buffer -> Maybe (Int, T.Text, [T.Text])
getBufferLastMatch buf =
  (cachedMatches . lastSearchPhrase) buf !? (nextMatch . lastSearchPhrase) buf
