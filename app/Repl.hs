module Turing.Repl(
                  ) where

{- This module is intended to be only used in REPL while manually
playing around. Contains unsafe functions. -}

import qualified Turing.Interpreter as I
import qualified Turing.Machine as M
import qualified Turing.Parser as P

import qualified Text.Megaparsec as Mg


fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe _ = error "fromMaybe failed"


fromEither :: Either String b -> b
fromEither (Right x) = x
fromEither (Left y) = error y


fileToMachine :: String -> String -> IO M.Machine
fileToMachine filename tape =
  do file <- readFile filename
     pm <- return . fromMaybe . (Mg.parseMaybe P.machine) $ file
     mm <- return . fromMaybe . I.makeMachine tape $ pm
     return mm


stepMachineIO :: M.Machine -> IO M.Machine
stepMachineIO m = return . fromEither . I.step $ m


stepTimes :: Int -> M.Machine -> IO M.Machine
stepTimes n m =
  if n <= 0
  then return m
  else stepMachineIO m >>= stepTimes (n - 1)


simulate :: String -> String -> Int -> IO ()
simulate f t n = fileToMachine f t >>= stepTimes n >>= printBrief


printBrief :: M.Machine -> IO ()
printBrief m = putStrLn ((reverse . M.tapeLeft $ m) ++ '[':(M.tapeCurrent m):']':(M.tapeRight m)
                         ++ "  " ++ (M.name . M.currentState $ m))
