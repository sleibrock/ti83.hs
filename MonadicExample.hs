module MonadicExample where

import Monadic
import Control.Monad


program = initP
  >>= clrHome
  >>= label "ExampleLabel"
  >>= for "A" (1, 10)
  (\s -> do
      initBlock s >>= disp "HELLO!")
  >>= end


main :: IO ()
main = do
  case program of
    Left e -> forM_ e (putStrLn . show)
    Right p -> do
      putStrLn "Words"
      putStrLn $ "State blob: " ++ (show p)
      putStrLn ""
      putStrLn "Resultant Code:"
      forM_ (tilines p) putStrLn
  
  putStrLn ""
  putStrLn "Done compiling"
