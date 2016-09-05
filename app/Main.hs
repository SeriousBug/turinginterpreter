module Main where

import qualified Turing.Interpreter as I
import qualified Turing.Machine as M
import qualified Turing.Parser as P

import Data.List (intercalate)
import Data.List.Extra (replace, trim)
import System.Environment (getArgs)
import qualified Text.Megaparsec as Mg


printUsage :: IO ()
printUsage =
  do putStrLn "Usage: turinginterpreter-exe <machine-file>"
     putStrLn "Example usage:"
     putStrLn "    $ echo 111 | turinginterpreter-exe machines/copy"


main :: IO ()
main =
  do args <- getArgs
     case length args of
       1 ->
         do input <- getLine
            let tape = replace " " "_" input
            machineFile <- readFile (head args)
            (putStrLn . cleanTape) (runMachine tape machineFile)
       _ -> printUsage


runMachine :: String -> String -> String
runMachine tape input =
  case Mg.runParser P.machine "" input of
    Right parsed -> I.stepUntilHalts (I.makeMachine tape parsed)
    Left errs -> showParseError errs



showParseError :: Mg.ParseError -> String
showParseError err =
  "Parsing error at " ++ errPos ++  ", found " ++ found ++ " but was expecting\n\t" ++ intercalate "\n\t" expects
  where errMsg = (map Mg.messageString . Mg.errorMessages) err
        found = head errMsg
        expects = tail errMsg
        errPos = (show . Mg.sourceLine) ep ++ ":" ++ (show . Mg.sourceColumn) ep
        ep = Mg.errorPos err


-- | To show a nicer representation of the tape, convert the
-- underscores with spaces and trim the spaces from both ends.
cleanTape :: String -> String
cleanTape = trim . replace "_" " "
