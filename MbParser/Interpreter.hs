module Main where

import System.Environment ( getArgs )
import System.IO ( stdin, hGetContents )

import AbsMbCore
import LexMbCore
import ParMbCore
import InterpretBody

import ErrM

runInterpreter :: String -> IO ()
runInterpreter input = case result of
    Ok s -> putStrLn s
    Bad s -> putStrLn $ "ERROR: " ++ s
  where result = do {
    abstractSyntax <- pBody $ myLexer input;
    interpretBody $ abstractSyntax;
  }

main :: IO ()
main = do
  args <- getArgs

  input <- case args of
    [] -> getContents
    (arg:_) -> readFile arg

  runInterpreter input
