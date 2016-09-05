module Turing.Machine
  ( Machine(..)
  , Symbol
  , Direction(..)
  , Transition(..)
  , State(..))
where


import Data.HashMap.Strict (HashMap)


data Machine = Machine
  { currentState :: !State -- ^ Current state the machine is in
  , tapeLeft :: ![Symbol] -- ^ The tape towards the left of the head
  , tapeRight :: ![Symbol] -- ^ The tape towards the right of the head
  , tapeCurrent :: !Symbol -- ^ The symbol machine head is currently on
  }
  deriving (Show)


data State = State
  { name :: !String -- ^ Name of the state
  , transitions :: HashMap Symbol Transition
  -- ^ The transitions that could apply when in this state, mapping
  -- the symbols that need to be read to the transitions themselves
  }
  deriving (Show)


type Symbol = Char


data Direction
  = LeftDirection
  | RightDirection
  deriving (Show)


data Transition = Transition
  { writeSym :: !Symbol -- ^ This symbol is written to tape when the transition applies
  , direction :: !Direction -- ^ The tape will move in this direction when the transition applies
  , toState :: State -- ^ The new state the machine will enter when the transition applies
  }
  deriving (Show)
