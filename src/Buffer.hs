{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Buffer
  ( Buffer(..)
  , emptyBuffer
  , initialBuffer
  , insertBuffer
  , getBufferLine
  , getBufferLines
  , incCurrentLine
  , decCurrentLine
  , deleteFromBuffer
  )
where

import           Data.Vector                    ( (!?)
                                                , (//)
                                                )
import qualified Data.Text                     as T
import qualified Data.Vector                   as Vector


data Buffer = Buffer
  { bufferFileName :: String
  , cursorPosition :: Int
  , markedPosition :: Int
  , bufferContent  :: Vector.Vector T.Text
  }
  deriving (Eq, Show)

emptyBuffer :: Buffer
emptyBuffer = Buffer "" 0 0 Vector.empty

initialBuffer :: String -> T.Text -> Buffer
initialBuffer name body = Buffer name cp 0 content
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

getBufferLine :: Buffer -> Int -> Maybe T.Text
getBufferLine = fmap (!?) bufferContent

-- TODO: Should return Either T.Text (Vector.Vector T.Text)
getBufferLines :: Buffer -> Int -> Int -> Maybe (Vector.Vector T.Text)
getBufferLines buf from to
  | Vector.length bc > to = Just $ Vector.slice (from - 1) to bc
  | otherwise             = Nothing
  where bc = bufferContent buf

incCurrentLine :: Buffer -> Buffer
incCurrentLine buf = buf { cursorPosition = cursorPosition buf + 1 }

decCurrentLine :: Buffer -> Buffer
decCurrentLine buf = buf { cursorPosition = cursorPosition buf - 1 }

-- TODO: Probably it would be better (in terms of delete operation performance)
--       to store lines as Vector Maybe String and removing them by inserting
--       Nothing.
deleteFromBuffer :: Buffer -> Buffer
deleteFromBuffer buf = buf
  { bufferContent = uncurry
                      (<>)
                      (Vector.splitAt (cursorPosition buf) (bufferContent buf))
  }
