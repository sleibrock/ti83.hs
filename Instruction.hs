module Instruction where

import Funcs
import Error

data ArithE = INT Int
            | FLT Float 
            | VAR String
            | Add ArithE ArithE
            | Sub ArithE ArithE
            | Mul ArithE ArithE
            | Div ArithE ArithE
            | Eq ArithE ArithE
            | Lt ArithE ArithE
            | Gt ArithE ArithE
            | Lte ArithE ArithE
            | Gte ArithE ArithE
            deriving (Eq)

data Instruction = ClrHome
                 | Break
                 | Stop
                 | Pause
                 | End
                 | Display String
                 | Output  Int Int String
                 | Label   String
                 | Goto    String
                 | Prompt  String
                 | Input   String
                 | Menu    [String]
                 | Set     String ArithE 
                 | For     String (Int, Int, Int) [Instruction]
                 | While   String (Int, Int, Int) [Instruction]
                 deriving (Eq)


instance Show ArithE where
  show (INT x)   = show x
  show (FLT x)   = show x
  show (VAR s)   = s
  show (Add l r) = join ["(", (show l), "+", (show r), ")"]
  show (Sub l r) = join ["(", (show l), "-", (show r), ")"]
  show (Mul l r) = join ["(", (show l), "*", (show r), ")"]
  show (Div l r) = join ["(", (show l), "/", (show r), ")"]
  show (Eq l r)  = join ["(", (show l), "=", (show r), ")"]
  show (Lt l r)  = join ["(", (show l), "<", (show r), ")"]
  show (Gt l r)  = join ["(", (show l), ">", (show r), ")"]
  show (Lte l r) = join ["(", (show l), "<=", (show r), ")"]
  show (Gte l r) = join ["(", (show l), ">=", (show r), ")"]


-- Implement Show instance for the entire Instruction set
-- Evaluatees like FOR or WHILE are more complex and not
-- defined here, see the evaluation section
instance Show Instruction where
  show ClrHome        = "ClrHome"
  show Break          = "Stop"
  show Stop           = "Stop"
  show Pause          = "Pause"
  show End            = "End"
  show (Display s)    = "Disp " ++ s
  show (Output x y s) = join ["Output(", (show x), ",", (show y), ",\"", s, "\")"]
  show (Label s)      = "LBL " ++ s
  show (Goto s)       = "GOTO " ++ s
  show (Prompt s)     = "Prompt " ++ s
  show (Input s)      = "Input " ++ s
  show (Set s e)      = join [(show e), "->", s]
  show (For s (b, e, i) _) = join ["FOR(",s, ",", (show b), ",", (show e), ",", (show i), ")"]




-- new and improved TI-83 state accumulator type
data TIState = TIState
  { tilines :: [String]
  , tilabels :: [String]
  , tivars :: [String]
  , ticount :: Int
  } deriving (Show, Eq)



type TIEither = Either [TIError] TIState

mkErrStr :: TIState -> [String] -> String
mkErrStr s lst = join $ lst ++ [" (line ", (show linum), ")"]
  where linum = (ticount s) + 1


initState :: TIState
initState = TIState {
  tilines  = [],
  tilabels = [],
  tivars   = [],
  ticount  = 0
  }

addLine :: TIState -> [String] -> TIState
addLine initS new_line = initS {
  tilines = (tilines initS) ++ new_line,
  ticount = (ticount initS) + (length new_line)
  }

addData :: TIState -> [String] -> [String] -> [String] -> TIState
addData initS lines lbls vars = initS {
  tilines = (tilines initS) ++ lines,
  tilabels = (tilabels initS) ++ lbls,
  tivars = (tivars initS) ++ vars,
  ticount = (ticount initS) + (length lines)
  }


findVars :: ArithE -> [String]
findVars (VAR x)   = [x]
findVars (Add l r) = (findVars l) ++ (findVars r)
findVars (Sub l r) = (findVars l) ++ (findVars r)
findVars (Mul l r) = (findVars l) ++ (findVars r)
findVars (Div l r) = (findVars l) ++ (findVars r)
findVars (Eq  l r) = (findVars l) ++ (findVars r)
findVars (Lt  l r) = (findVars l) ++ (findVars r)
findVars (Gt  l r) = (findVars l) ++ (findVars r)
findVars (Lte l r) = (findVars l) ++ (findVars r)
findVars (Gte l r) = (findVars l) ++ (findVars r)
findVars _         = []

simplify :: ArithE -> ArithE
simplify (Add (INT l) (INT r)) = INT (l + r)
simplify (Add (INT l) (FLT r)) = FLT ((fromIntegral l) + r)
simplify (Add (FLT l) (FLT r)) = FLT (l + r)
simplify (Sub (INT l) (INT r)) = INT (l - r)
simplify (Sub (INT l) (FLT r)) = FLT ((fromIntegral l) - r)
simplify (Sub (FLT l) (FLT r)) = FLT (l - r)
simplify (Mul (INT 0) (INT _)) = INT 0
simplify (Mul (INT _) (INT 0)) = INT 0
simplify (Mul (INT 1) (INT x)) = INT x
simplify (Mul (INT x) (INT 1)) = INT x
simplify (Div x       (INT 1)) = x
simplify (Eq  l r) = Eq  (simplify l) (simplify r)
simplify (Lt  l r) = Lt  (simplify l) (simplify r)
simplify (Gt  l r) = Gt  (simplify l) (simplify r)
simplify (Lte l r) = Lte (simplify l) (simplify r)
simplify (Gte l r) = Gte (simplify l) (simplify r)
simplify (Add l r) = Add (simplify l) (simplify r)
simplify (Sub l r) = Sub (simplify l) (simplify r)
simplify (Mul l r) = Mul (simplify l) (simplify r)
simplify (Div l r) = Div (simplify l) (simplify r)
simplify x = x

findErrors :: ArithE -> Maybe String
findErrors (Div _ (INT 0))   = Just "Division by zero detected"
findErrors (Div _ (FLT 0.0)) = Just "Division by zero detected"
findErrors _ = Nothing

-- end Instruction.hs
