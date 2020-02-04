module Evaluator
  ( module Evaluator
  , module Instruction
  , module Error
  ) where


import Funcs
import Error
import Instruction
import Data.List (elem)

parseNext :: TIState -> Instruction -> Either [TIError] TIState
parseNext s inst@(Label str) =
  Right $ addData s [(show inst)] [str] []

parseNext s inst@(Goto str) =
  if elem str (tilabels s)
  then Right $ addLine s [(show inst)]
  else Left $ [GotoError $ mkErrStr s ["No label '", str, "' found"]]

-- Parse a FOR instruction (hard issue)
parseNext s inst@(For var _ code) =
  if elem var (tivars s)
  then Left $ [LoopError $ mkErrStr s ["Variable '", var, "' in use already"]]
  else case evaluate copyS code of
    Left e -> Left $ [LoopError $ mkErrStr s ["Loop scope error occured"]] ++ e
    Right s' -> Right $ addData s
      ([(show inst)] ++ (tilines s') ++ [(show End)])
      (tilabels s')
      (tivars s')
  where copyS = TIState {
          tilines = [], tilabels = [],
          tivars = (tivars s), ticount = (succ $ ticount s)}
                       
-- base parser rule
parseNext s i = Right $ addLine s [(show i)]


-- use this if you want to evaluate with a specific state
evaluate :: TIState -> [Instruction] -> Either [TIError] TIState 
evaluate init ilst = ev ilst init
  where ev []     s = Right s
        ev (i:is) s = case parseNext s i of
          Left e -> Left e
          Right s' -> ev is s'


-- quicker short-hand function using the initState var
eval :: [Instruction] -> Either [TIError] TIState
eval = evaluate initState


-- end Evaluator
