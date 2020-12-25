{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Event
  ( Address(..)
  , EditorAction(..)
  , Direction(..)
  , run
  )
where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Vector                   as Vector
import           Text.Printf                    ( printf )

import           Buffer

data Address
  = Lines { fromLine :: Int, toLine :: Int}
  | LinesFromCurrentToLast
  | AllLines
  | Line Int
  | CurrentLine
  | LastLine
  | PreviousLine
  | NextLine
  | NextRegexLine T.Text
  | PrevRegexLine T.Text
  | MarkedLine Int
  deriving (Eq, Show)

data EditorAction
  = Append
  | Insert Address
  | Delete Address
  | Debug
  | Search Direction String
  | NLine
  | PLine
  | Print Address
  | Error
  | SaveFile (Maybe FilePath)
  | Quit
  deriving (Eq, Show)

data Direction
  = Backward
  | Forward
  deriving (Eq, Show)


run :: Buffer -> EditorAction -> IO (Maybe Buffer)
run buf event = case event of
  Append                 -> runInsert buf CurrentLine
  Insert address         -> runInsert buf address
  Delete _address        -> return $ Just $ deleteFromBuffer buf
  Debug                  -> print buf >> return Nothing
  Search Backward _match -> putStrLn "Not Implemented" >> return Nothing
  Search Forward  _match -> putStrLn "Not Implemented" >> return Nothing
  NLine                  -> return $ Just $ incCurrentLine buf
  PLine                  -> return $ Just $ decCurrentLine buf
  Print address -> print address >> runPrint buf address >> return Nothing
  Error                  -> return Nothing
  SaveFile maybeFilePath -> runSaveFile buf maybeFilePath >> return Nothing
  Quit                   -> return Nothing

runSaveFile :: Buffer -> Maybe FilePath -> IO ()
runSaveFile buf filePath = case filePath of
  Nothing -> TIO.writeFile (bufferFileName buf)
                           (T.unlines . Vector.toList $ bufferContent buf)
  Just name ->
    TIO.writeFile name (T.unlines . Vector.toList $ bufferContent buf)

runInsert :: Buffer -> Address -> IO (Maybe Buffer)
runInsert buf address =
  go []
    >>= (\content -> case address of
          CurrentLine -> return $ extendBuffer content (cursorPosition buf) buf
          Line number -> return $ extendBuffer content (number - 1) buf
          _           -> putStrLn "?" >> return Nothing
        )
    .   reverse
 where
  go :: [T.Text] -> IO [T.Text]
  go acc =
    TIO.getLine
      >>= (\case
            "."   -> return acc
            other -> go (other : acc)
          )

runPrint :: Buffer -> Address -> IO ()
runPrint buf address = case address of
  Lines from to -> case getBufferLines buf from to of
    Just xs -> mapM_ (printf "%s\n") xs
    Nothing -> putStrLn "?"
  AllLines -> mapM_ (printf "%s\n") (bufferContent buf)
  Line n   -> case getBufferLine buf n of
    Just x  -> printf "%s\n" x
    Nothing -> putStrLn "?"
  CurrentLine -> case getBufferLine buf (cursorPosition buf) of
    Just x  -> printf "%s\n" x
    Nothing -> putStrLn "?"
  LastLine -> case getBufferLine buf $ Vector.length (bufferContent buf) - 1 of
    Just x  -> printf "%s\n" x
    Nothing -> putStrLn "?"
  PreviousLine -> case getBufferLine buf $ cursorPosition buf - 1 of
    Just x  -> printf "%s\n" x
    Nothing -> putStrLn "?"
  NextLine -> case getBufferLine buf $ cursorPosition buf - 1 of
    Just x  -> printf "%s\n" x
    Nothing -> putStrLn "?"
  NextRegexLine _regex -> putStrLn "Not Implemented"
  PrevRegexLine _regex -> putStrLn "Not Implemented"
  MarkedLine    n      -> case getBufferLine buf n of
    Just x  -> printf "%s\n" x
    Nothing -> putStrLn "?"
