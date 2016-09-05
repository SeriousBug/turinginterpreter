module Turing.Interpreter
  ( makeMachine
  , step
  , stepUntilHalts
  , extractTape
  ) where


import qualified Turing.Machine as M

import qualified Data.HashMap.Strict as HM


-- | Continuously steps the machine until it halts, then returns the
-- final tape.
stepUntilHalts :: M.Machine -> [M.Symbol]
stepUntilHalts machine =
  case step machine of
    Left tape -> tape
    Right newmachine -> stepUntilHalts newmachine


-- | Moves the machine one step forward by reading a symbol from the
-- tape and applying the appropriate transition. If there is no
-- suitable transition, the machine halts and the tape is returned.
step :: M.Machine -> Either [M.Symbol] M.Machine
step machine =
  case transition of
    Nothing -> Left (extractTape machine)
    Just t -> Right (applyTransition machine t)
  where transition = r `HM.lookup` (M.transitions cs) 
        r = M.tapeCurrent machine
        cs = M.currentState machine


-- | Applies the transition to the machine, writing the symbol and
-- moving the tape.
applyTransition :: M.Machine -> M.Transition -> M.Machine
applyTransition machine transition =
    (move (M.direction transition)
     (machine { M.tapeCurrent = M.writeSym transition
              , M.currentState = M.toState transition}))

-- | Move machines tape in the direction. Note that it is the tape and
-- not the head being moved; a 'M.RightDirection' moves the tape to
-- the right, which means the head moves to the left.
move ::  M.Direction -> M.Machine -> M.Machine
move direction machine =
  let left = M.tapeLeft machine
      cur = M.tapeCurrent machine
      right = M.tapeRight machine
  in case direction of
       M.LeftDirection ->
         let (newleft, newcur, newright) = moveTape left cur right
         in machine { M.tapeLeft = newleft, M.tapeCurrent = newcur, M.tapeRight = newright }
       M.RightDirection ->
         let (newright, newcur, newleft) = moveTape right cur left
         in machine { M.tapeLeft = newleft, M.tapeCurrent = newcur, M.tapeRight = newright }


moveTape :: [M.Symbol] -> M.Symbol -> [M.Symbol] -> ([M.Symbol], M.Symbol, [M.Symbol])
moveTape back cur forw =
  let (newcur, newforw) = pullTape forw
  in (cur:back, newcur, newforw)


-- | Given a tape and states, creates a machine. The first state in
-- the state list will be used as the initial state.
makeMachine :: [M.Symbol] -> M.State -> M.Machine
makeMachine tape state =
  let (t, ts) = pullTape tape
  in M.Machine state [] ts t


-- | Returns the tape in the machine.
extractTape :: M.Machine -> [M.Symbol]
extractTape machine =
  (reverse . M.tapeLeft) machine ++ M.tapeCurrent machine : M.tapeRight machine


-- | Accepts a tape, or a segment of the tape. Returns the head and
-- tail of the tape. If the tape is empty, the head will be an
-- underscore.
pullTape :: [M.Symbol] -> (M.Symbol, [M.Symbol])
pullTape (t:ts) = (t, ts)
pullTape [] = ('_', [])
