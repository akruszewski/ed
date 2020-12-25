{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module App
  ( runApp
  )
where

import           Control.Monad                  ( void )
import           Data.Functor                   ( (<&>) )
import qualified Data.Text.IO                  as TIO
import           System.Directory               ( doesFileExist )
import           System.Environment             ( getArgs )

import           Buffer
import           Parser
import           Event

runApp :: IO ()
runApp = getArgs >>= createBuffer >>= runEditor >> return ()

runEditor :: Buffer -> IO ()
runEditor buf = getLine >>= \input -> do
  case toAction input of
    Just Quit   -> return ()
    Just action -> run buf action >>= \case
      Just nbuf -> void $ runEditor nbuf
      Nothing   -> void $ runEditor buf
    Nothing -> void $ putStrLn "?"

createBuffer :: [String] -> IO Buffer
createBuffer = \case
  []     -> return emptyBuffer
  [name] -> doesFileExist name >>= \exist -> if exist
    then TIO.readFile name <&> initialBuffer name
    else return $ initialBuffer name ""
