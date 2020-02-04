module Instruction where

import Funcs
import Error

data Node = NAN
          | INT Int
          | FLT Float
          | VAR String
          deriving (Eq)

data ArithE = ID Node
            | Add Node Node
            | Sub Node Node
            | Mul Node Node
            | Div Node Node
            | Eq Node Node
            | Lt Node Node
            | Gt Node Node
            | Lte Node Node
            | Gte Node Node
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

-- Implement Show instance for the raw data types
instance Show Node where
  show NAN = "NaN"
  show (INT x) = show x
  show (FLT x) = show x
  show (VAR x) = x


instance Show ArithE where
  show (ID x)    = show x
  show (Add l r) = join [(show l), "+", (show r)]
  show (Sub l r) = join [(show l), "-", (show r)]
  show (Mul l r) = join [(show l), "*", (show r)]
  show (Div l r) = join [(show l), "/", (show r)]
  show (Eq l r)  = join [(show l), "=", (show r)]
  show (Lt l r)  = join [(show l), "<", (show r)]
  show (Gt l r)  = join [(show l), ">", (show r)]
  show (Lte l r) = join [(show l), "<=", (show r)]
  show (Gte l r) = join [(show l), ">=", (show r)]


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


-- end Instruction.hs
