module Turing.Parser
  ( machine ) where

import qualified Turing.Machine as M

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Text.Megaparsec as Mg
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String



machine :: Parser [M.State]
machine = sc *> Mg.some state <* Mg.eof


state :: Parser M.State
state =
  do n <- stateName
     ts <- brackets (Mg.many transition)
     return (M.State n ts)


transition :: Parser M.Transition
transition =
  do readSymbols <- Mg.some tapeSymbol
     arrow
     writeSymbol <- tapeSymbol
     semicolon
     moveDirection <- direction
     semicolon
     newState <- stateName
     return (M.Transition readSymbols writeSymbol moveDirection newState)


direction :: Parser M.Direction
direction = lexeme (leftDirection Mg.<|> rightDirection)
  where leftDirection =
          do Mg.char 'R'
             return M.RightDirection
        rightDirection =
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
