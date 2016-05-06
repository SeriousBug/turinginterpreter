module Turing.Machine
  ( Machine(..)
  , Symbol
  , Direction(..)
  , Transition(..)
  , State(..))
where


data Machine = Machine
  { currentState :: !State -- ^ Current state the machine is in
  , states :: ![State] -- ^ All possible states of this machine
  , tapeLeft :: ![Symbol] -- ^ The tape towards the left of the head
  , tapeRight :: ![Symbol] -- ^ The tape towards the right of the head
  , tapeCurrent :: !Symbol -- ^ The symbol machine head is currently on
  }
  deriving (Show)


data State = State
  { name :: !String -- ^ Name of the state
  , transitions :: ![Transition] -- ^ The transitions that could apply when in this state
  }
  deriving (Show)



type Symbol = Char


data Direction
  = LeftDirection
  | RightDirection
  deriving (Show)


data Transition = Transition
  { readSyms :: ![Symbol] -- ^ The transition applies if these symbols are read
  , writeSym :: !Symbol -- ^ This symbol is written when the transition applies
  , direction :: !Direction -- ^ The tape will move in this direction when transition applies
  , toState :: !String -- ^ The new state will be the state with this name when transition applies
  }
  deriving (Show)

