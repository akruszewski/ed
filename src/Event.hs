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
import           Text.RE.TDFA.Text              ( (*=~)
                                                , RE(..)
                                                , compileRegex
                                                , matches
                                                )

import           Buffer
import           Direction

data Address
  = Lines { fromLine :: Int, toLine :: Int}
  | LinesFromCurrentToLast
  | AllLines
  | Line Int
  | CurrentLine
  | LastLine
  | PreviousLine
  | NextLine
  | MarkedLine Int
  deriving (Eq, Show)

data EditorAction
  = Append
  | Insert Address
  | Delete Address
  | Debug
  | Search Direction T.Text
  | NLine
  | PLine
  | Print Address
  | Error
  | SaveFile (Maybe FilePath)
  | Quit
  deriving (Eq, Show)

run :: Buffer -> EditorAction -> IO (Maybe Buffer)
run buf event = case event of
  Append                 -> runInsert buf CurrentLine
  Insert address         -> runInsert buf address
  Delete _address        -> return $ Just $ deleteFromBuffer buf
  Debug                  -> print buf >> return Nothing
  Search direction match -> runSearch buf direction match
  NLine                  -> return $ Just $ incCurrentLine buf
  PLine                  -> return $ Just $ decCurrentLine buf
  Print address          -> runPrint buf address >> return Nothing
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

runSearch :: Buffer -> Direction -> T.Text -> IO (Maybe Buffer)
runSearch buf direction regex
  | phrase (lastSearchPhrase buf) == regex = do
    case getBufferLastMatch buf of
      Just match -> do
        printMatch match
        return $ Just buf
      Nothing -> runSearch (buf { lastSearchPhrase = emptySearchPhrase })
                           direction
                           regex
    return $ Just (updateBufferNextMatch direction buf)
  | otherwise = case regexCompiled regex of
    Just rc -> do
      let res = runMatch (fromCurrentPosition buf direction) rc
      let (idx, line, _) = Vector.head res
      printf "%d\t| %s\n" (idx + 1) line
      return $ Just buf
        { lastSearchPhrase = SearchPhrase
                               regex
                               (if Vector.length res > 1 then idx + 1 else idx)
                               res
        , cursorPosition   = idx
        }
    Nothing -> putStrLn "Invalid Regex" >> return Nothing
 where

  printMatch :: (Int, T.Text, [T.Text]) -> IO ()
  printMatch (idx, line, _) = printf "%d\t| %s\n" (idx + 1) line

  runMatch
    :: Vector.Vector (Int, T.Text)
    -> RE
    -> Vector.Vector (Int, T.Text, [T.Text])
  runMatch xs rc = Vector.filter haveMatch $ xs >>= match rc

  haveMatch :: (Int, T.Text, [T.Text]) -> Bool
  haveMatch (_, _, []) = False
  haveMatch (_, _, _ ) = True

  match :: RE -> (Int, T.Text) -> Vector.Vector (Int, T.Text, [T.Text])
  match rc (x, txt) = return (x, txt, matches $ txt *=~ rc)

  regexCompiled :: T.Text -> Maybe RE
  regexCompiled regex = compileRegex $ T.unpack regex

  fromCurrentPosition :: Buffer -> Direction -> Vector.Vector (Int, T.Text)
  fromCurrentPosition buf dir
    | dir == Forward = Vector.concat [after, before]
    | dir == Backward = Vector.concat
      [Vector.reverse before, Vector.reverse after]
   where
    (before, after) =
      Vector.splitAt (cursorPosition buf) (Vector.indexed $ bufferContent buf)

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
  MarkedLine n -> case getBufferLine buf n of
    Just x  -> printf "%s\n" x
    Nothing -> putStrLn "?"
