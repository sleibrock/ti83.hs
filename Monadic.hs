module Monadic
  ( module Monadic 
  , module Instruction
  , module Funcs
  , module Error
  ) where

import Funcs
import Error
import Instruction
import Data.List (elem)


-- Initialize our program with some empty state
initP :: TIEither
initP = Right $ initState


clrHome :: TIState -> TIEither
clrHome s = Right $ addLine s [show ClrHome] 


end :: TIState -> TIEither 
end s = Right $ addLine s [show End]


label :: String -> TIState -> TIEither 
label str s =
  if elem str (tilabels s)
  then Left $ [LabelError $ mkErrStr s []]
  else Right $ addData s [show (Label str)] [str] []

goto :: String -> TIState -> TIEither
goto str s =
  if elem str (tilabels s)
  then Right $ addLine s [show (Goto str)]
  else Left $ [GotoError $ mkErrStr s []]


disp :: String -> TIState -> TIEither 
disp str s = Right $ addLine s [show (Display str)]


output :: (Int, Int) -> String -> TIState -> TIEither
output (x, y) str s = Right $ addLine s [show (Output x y str)]



-- Should only be used when evaluating conditional code
-- Used for FOR, WHILE, IF/ELSE
initBlock :: TIState -> Either [TIError] TIState
initBlock s = Right s


-- Evaluate a FOR loop level macro
-- To do this, we must be supplied with a function that accepts an initial state
-- The inner FOR loop code will be considered a chained monadic function
-- that passes a copy of some state and returns the final state
-- We copy the resultant state data into our original state
--
-- str    - name of the Symbol to use
-- (a, b) - indexes of the loop to control (from [a..b])
-- f      - a (TIState -> Either [TIError] TIState) function
-- s      - our chained state we are working with

for :: String -> (Int, Int) -> (TIState -> TIEither) -> TIState -> TIEither
for str (a, b) f s =
  if elem str (tivars s)
  then Left $ [LoopError $ mkErrStr s ["Variable '", str, "' in use already"]]
  else case s' of
    Left e -> Left $ [LoopError "oops"] ++ e
    Right newS -> Right $ addData s
       ([(show (For str (a, b, 1) []))] ++ (tilines newS) ++ [(show End)])
       (tilabels newS)
       (tivars newS)
  where s' = f copyS
        copyS = TIState {
          tilines = [], tilabels = [],
          tivars = (tivars s), ticount = (succ $ ticount s)}


-- Same as above, except with three FOR loop parameters
--
-- (a, b, i) - indexes of the loop, but i determines by how much we increment
forI :: String -> (Int, Int, Int) -> (TIState -> TIEither) -> TIState -> TIEither
forI str (a, b, i) f s =
  if elem str (tivars s)
  then Left $ [LoopError $ mkErrStr s ["Variable '", str, "' in use already"]]
  else case s' of
    Left e -> Left $ [LoopError "oops"] ++ e
    Right newS -> Right $ addData s
       ([(show (For str (a, b, i) []))] ++ (tilines newS) ++ [(show End)])
       (tilabels newS)
       (tivars newS)
  where s' = f copyS
        copyS = TIState {
          tilines = [], tilabels = [],
          tivars = (tivars s), ticount = (succ $ ticount s)}





-- end
