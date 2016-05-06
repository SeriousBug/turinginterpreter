module Main where

import qualified Turing.Interpreter as I
import qualified Turing.Machine as M
import qualified Turing.Parser as P

import Data.List (intercalate)
import Data.List.Extra (replace, trim)
import System.Environment (getArgs)
import qualified Text.Megaparsec as Mg


main :: IO ()
main =
  do args <- getArgs
     let tape = intercalate "_" args
     interact (cleanTape . runMachine tape)


runMachine :: String -> String -> String
runMachine tape input =
  case Mg.runParser P.machine "" input of
    Right parsed ->
      case I.makeMachine tape parsed of
        Nothing -> "Error! The machine must contain at least one state."
        Just m -> I.stepUntilHalts m
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
