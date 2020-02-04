module Error where


data TIError = Nondescript
             | GotoError String
             | VarError String
             | LoopError String
             | ParseError String
             deriving (Eq)

instance Show TIError where
  show Nondescript   = "Error: undefined error"
  show (VarError e)  = "VarError: " ++ e
  show (GotoError e) = "GotoError: " ++ e
  show (LoopError e) = "LoopError: " ++ e
  show (ParseError e) = "ParseError: " ++ e


-- end TIError.hs
