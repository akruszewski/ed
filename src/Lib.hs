{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  )
where

import           Data.Functor                   ( (<&>) )
import           Data.Vector                    ( (!?)
                                                , (//)
                                                )
import qualified Data.Vector                   as Vector
import           System.Environment             ( getArgs )

data EditorAction
  = StartInsert
  | Delete
  | Print
  | NextLine
  | PrevLine
  | PrintAll
  | Error
  | SaveFile
  | Quit

data Buffer = Buffer
  { cursorPosition :: Int,
    currentLine :: String,
    bufferContent :: Vector.Vector String
  }
  deriving (Eq, Show)

someFunc :: IO ()
someFunc =
  getArgs
    >>= (\case
          []     -> return emptyBuffer
          [name] -> readFile name <&> initialBuffer
        )
    >>= run
    >>  return ()

run :: Buffer -> IO ()
run buf = getLine >>= \input -> case toAction input of
  StartInsert -> runInsert [] >>= run . foldr insertBuffer buf
  NextLine    -> run $ incCurrentLine buf
  PrevLine    -> run $ decCurrentLine buf
  Delete      -> run $ deleteFromBuffer buf
  PrintAll    -> do
    mapM_ putStrLn (bufferContent buf)
    run buf
  Print ->
    (case getBufferLine buf of
        Just x  -> putStrLn x
        Nothing -> putStrLn "Line not found."
      )
      >> run buf
  SaveFile ->
    writeFile "test.txt" (unlines . Vector.toList $ bufferContent buf)
      >> run buf
  Error -> putStrLn "?" >> run buf
  Quit  -> return ()

runInsert :: [String] -> IO [String]
runInsert acc =
  getLine
    >>= (\case
          "."   -> return acc
          other -> runInsert (other : acc)
        )

insertBuffer :: String -> Buffer -> Buffer
insertBuffer content buf = buf { bufferContent  = newBufferContent
                               , cursorPosition = cursorPosition buf + 1
                               }
 where
  cp               = cursorPosition buf
  oldContent       = bufferContent buf
  newBufferContent = if cp >= Vector.length oldContent
    then Vector.snoc oldContent content
    else bufferContent buf // [(cp, content)]

getBufferLine :: Buffer -> Maybe String
getBufferLine buf = bufferContent buf !? cursorPosition buf

incCurrentLine :: Buffer -> Buffer
incCurrentLine buf = buf { cursorPosition = cursorPosition buf + 1 }

decCurrentLine :: Buffer -> Buffer
decCurrentLine buf = buf { cursorPosition = cursorPosition buf - 1 }

deleteFromBuffer :: Buffer -> Buffer
deleteFromBuffer buf = buf
  { bufferContent = uncurry
                      (<>)
                      (Vector.splitAt (cursorPosition buf) (bufferContent buf))
  }

emptyBuffer :: Buffer
emptyBuffer = Buffer 0 "" Vector.empty

initialBuffer :: String -> Buffer
initialBuffer body = Buffer 0 "" $ Vector.fromList (lines body)

toAction :: String -> EditorAction
toAction = \case
  "p"  -> Print
  ",p" -> PrintAll
  "a"  -> StartInsert
  "q"  -> Quit
  "d"  -> Delete
  "++" -> NextLine
  "--" -> PrevLine
  "w"  -> SaveFile
  _    -> Error
