module Main where

import Lexer
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> replMode
    "-" : _ -> replMode
    file : _ -> do
      content <- readFile file
      pPrint $ runLexer lexer content
  where
    replMode :: IO ()
    replMode = do
      line <- getLine
      pPrint $ runLexer lexer line
      replMode
