{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Event
  ( Address(..)
  , EditorAction(..)
  , Direction(..)
  , run
  )
where

import           Buffer
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Vector                   as Vector
import           Direction
import           Text.Printf                    ( PrintfArg
                                                , printf
                                                )
import           Text.RE.TDFA.Text              ( RE
                                                , compileRegex
                                                , matches
                                                , (*=~)
                                                )

data Address
  = Lines {fromLine :: Int, toLine :: Int}
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
  = Append Address
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
run buf = \case
  Append address         -> runInsert buf address (1 -)
  Insert address         -> runInsert buf address (1 +)
  Delete address         -> runDelete buf address
  Debug                  -> print buf >> return Nothing
  Search direction match -> runSearch buf direction match
  NLine                  -> return $ Just $ incCurrentLine buf
  PLine                  -> return $ Just $ decCurrentLine buf
  Print address          -> runPrint buf address >> return Nothing
  Error                  -> return Nothing
  SaveFile maybeFilePath -> runSaveFile buf maybeFilePath >> return Nothing
  Quit                   -> return Nothing

runSaveFile :: Buffer -> Maybe FilePath -> IO ()
runSaveFile buf = maybe
  (TIO.writeFile (bufferFileName buf)
                 (T.unlines . Vector.toList $ bufferContent buf)
  )
  (\name -> TIO.writeFile name (T.unlines . Vector.toList $ bufferContent buf))

runInsert :: Buffer -> Address -> (Int -> Int) -> IO (Maybe Buffer)
runInsert buf address shift =
  go []
    >>= (\content -> case address of
          CurrentLine ->
            return $ extendBuffer content (shift $ cursorPosition buf) buf
          NextLine ->
            return $ extendBuffer content (shift $ cursorPosition buf + 1) buf
          Line number -> return $ extendBuffer content (shift $ number - 1) buf
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
  | phrase (lastSearchPhrase buf) == regex
  = maybe
      (runSearch (buf { lastSearchPhrase = emptySearchPhrase }) direction regex)
      handleMatch
    $ getBufferLastMatch buf
  | otherwise
  = maybe (putStrLn "Invalid Regex" >> return Nothing) handleRegex
    $ regexCompiled regex
 where

  handleMatch :: (Int, T.Text, [T.Text]) -> IO (Maybe Buffer)
  handleMatch m =
    printMatch m >> return (Just $ updateBufferNextMatch direction buf)

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

  handleRegex :: RE -> IO (Maybe Buffer)
  handleRegex rc = do
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

  regexCompiled :: T.Text -> Maybe RE
  regexCompiled x = compileRegex $ T.unpack x

  fromCurrentPosition :: Buffer -> Direction -> Vector.Vector (Int, T.Text)
  fromCurrentPosition b dir
    | dir == Forward = Vector.concat [after, before]
    | dir == Backward = Vector.concat
      [Vector.reverse before, Vector.reverse after]
   where
    (before, after) =
      Vector.splitAt (cursorPosition b) (Vector.indexed $ bufferContent b)


runPrint :: Buffer -> Address -> IO ()
runPrint buf AllLines = mapM_ printf' (bufferContent buf)
runPrint buf (Lines from to) =
  maybe' (mapM_ printf') $ getBufferLines buf from to
runPrint buf LinesFromCurrentToLast = maybe' (mapM_ printf') $ getBufferLines
  buf
  (cursorPosition buf)
  (Vector.length (bufferContent buf) - 1)
runPrint buf address = maybePrint $ getBufferLine buf $ case address of
  Line n                 -> n
  CurrentLine            -> cursorPosition buf
  LastLine               -> Vector.length (bufferContent buf) - 1
  PreviousLine           -> cursorPosition buf - 1
  NextLine               -> cursorPosition buf - 1
  MarkedLine n           -> n

 -- TODO: to complete matching, not the best idea imho, need to figure out how to tackle it in propoer way
  AllLines               -> -1
  Lines _ _              -> -1
  LinesFromCurrentToLast -> -1
  where maybePrint = maybe' printf'

maybe' :: (a -> IO ()) -> Maybe a -> IO ()
maybe' = maybe $ putStrLn "?"

printf' :: PrintfArg a => a -> IO ()
printf' = printf "%s\n"

runDelete :: Buffer -> Address -> IO (Maybe Buffer)
runDelete buf address = return $ Just $ case address of
  Lines from to          -> deleteLinesFromBuffer buf (from - 1) to
  LinesFromCurrentToLast -> deleteLinesFromBuffer
    buf
    (cursorPosition buf)
    (Vector.length (bufferContent buf) - 1)
  AllLines -> deleteLinesFromBuffer buf 0 (Vector.length (bufferContent buf))
  Line n       -> deleteLineFromBuffer buf $ n - 1
  CurrentLine  -> deleteLineFromBuffer buf (cursorPosition buf)
  LastLine -> deleteLineFromBuffer buf (Vector.length (bufferContent buf) - 1)
  PreviousLine -> deleteLineFromBuffer buf $ cursorPosition buf - 1
  NextLine     -> deleteLineFromBuffer buf $ cursorPosition buf + 1
  MarkedLine n -> deleteLineFromBuffer buf $ n - 1

