module Main where

import Evaluator
import Control.Monad (forM_)

myProgram = [
  ClrHome,
  Output 3 2 "Hello World!",
  For "A" (1, 10, 1)
    [
      Goto "B"
    ],
  End
  ]

main :: IO ()
main = do
  let p = eval myProgram

  case p of
    Left e -> forM_ e (putStrLn . show) 
    Right s -> do
      putStrLn $ "State blob: " ++ (show s)
      putStrLn ""
      putStrLn "Resultant code:"
      forM_ (tilines s) putStrLn

  putStrLn ""
  putStrLn "-- End TI-83 builder"

-- end
