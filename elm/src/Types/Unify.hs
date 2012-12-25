
module Types.Unify (unify) where

import Control.Monad (liftM)
import qualified Data.Map as Map

import Guid
import Types.Constrain
import Types.Solver

import System.IO.Unsafe

prints xs v =
    unsafePerformIO (putStrLn "~~~~~~~~~~" >> mapM print xs) `seq` v

unify hints modul = run $ do
  constraints <- constrain hints modul
  case constraints of
    Left msg -> return (Left msg)
    Right (escapees, cs) ->
        prints cs $
        do subs <- solver cs Map.empty
           return $ do ss <- subs
                       prints (Map.toList ss) $ return (escapees, ss)

