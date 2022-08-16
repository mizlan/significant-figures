module Main where

import Data.SigFig (processExpression)
import System.Console.Haskeline
import qualified Data.Text as T

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      inp <- getInputLine "expr> "
      case inp of
        Nothing -> return ()
        Just expr -> do
          outputStrLn . T.unpack . processExpression . T.pack $ expr
          loop
