{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Turing.Parser
  ( machine ) where

import qualified Turing.Machine as M

import Control.Applicative (empty)
import Control.Monad (void)
import Data.HashMap.Strict (HashMap, lookup, fromList, (!))
import qualified Text.Megaparsec as Mg
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String


type PartialTransition = (M.Symbol, M.Symbol, M.Direction, String)
-- ^ Used to represent transitions as they are being parsed, before we
-- can tie the knot

type PartialState = (String, [PartialTransition])
-- ^ Used to represent states as they are being parsed, before we
-- can tie the knot

machine :: Parser M.State
machine = sc *> m <* Mg.eof
  where m = do s <- Mg.some state
               return $ tieMachine s


-- | Takes the list of partial states that were just parsed, and
-- builds an actual state from these partial states. The state
-- returned is the first state in the file, which will be the initial
-- state of the macine. The other states that were parsed are not
-- directly accessible, they can be accessed through the `toState`s of
-- the transitions.
tieMachine :: [PartialState] -> M.State
tieMachine states = (stateMap!) . fst . head $ states
    where stateMap :: HashMap String M.State
          stateMap = fromList . map tieState $ states
           
          tieState :: (String, [PartialTransition]) -> (String, M.State)
          tieState (name, ts) = (name, M.State name (tieTransitions ts))

          tieTransitions :: [PartialTransition] -> HashMap M.Symbol M.Transition
          tieTransitions = fromList . map tieTransition

          tieTransition :: PartialTransition -> (M.Symbol, M.Transition)
          tieTransition (rsym, wsym, dir, nm) = (rsym, M.Transition wsym dir (stateMap ! nm))


state :: Parser PartialState
state =
  do n <- stateName
     ts <- brackets (Mg.many transition)
     return (n, ts)


transition :: Parser PartialTransition
transition =
  do readSymbol <- tapeSymbol
     arrow
     writeSymbol <- tapeSymbol
     semicolon
     moveDirection <- direction
     semicolon
     newState <- stateName
     return (readSymbol, writeSymbol, moveDirection, newState)


direction :: Parser M.Direction
direction = lexeme (leftDirection Mg.<|> rightDirection)
  where rightDirection =
          do Mg.char 'R'
             return M.RightDirection
        leftDirection =
          do Mg.char 'L'
             return M.LeftDirection


brackets :: Parser a -> Parser a
brackets = Mg.between (symbol "{") (symbol "}")


arrow :: Parser String
arrow = lexeme (symbol "->")


semicolon :: Parser String
semicolon = lexeme (symbol ",")


stateName :: Parser String
stateName = lexeme (Mg.some C.alphaNumChar)


tapeSymbol :: Parser M.Symbol
tapeSymbol = lexeme (C.alphaNumChar Mg.<|> C.char '_')


symbol :: String -> Parser String
symbol = L.symbol sc


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


sc :: Parser ()
sc = L.space (void Mg.spaceChar) (L.skipLineComment "#") empty
