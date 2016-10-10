{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Pattern (Info(..), joinInfos, infoToScheme, constrain) where

import Control.Arrow (second)
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map as Map

import qualified AST.Pattern as P
import qualified AST.Variable as V
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Type.Constrain.Literal as Literal
import qualified Type.Environment as Env
import Type.Type



-- PATTERN INFO


data Info =
  Info
    { _headers :: Map.Map String (A.Located Type)
    , _vars :: [Variable]
    , _cons :: TypeConstraint
    }


emptyInfo :: Info
emptyInfo =
    Info Map.empty [] CTrue


joinInfo :: Info -> Info -> Info
joinInfo (Info headers1 vars1 cons1) (Info headers2 vars2 cons2) =
    Info
      { _headers = Map.union headers1 headers2
      , _vars = vars1 ++ vars2
      , _cons = cons1 /\ cons2
      }


joinInfos :: [Info] -> Info
joinInfos infos =
    List.foldl' (flip joinInfo) emptyInfo infos


infoToScheme :: Info -> Scheme Type Variable
infoToScheme (Info headers vars cons) =
    Scheme [] vars cons headers



-- CONSTRAIN PATTERN


constrain :: Env.Env -> P.Canonical -> Type -> IO Info
constrain env (A.A region pattern) tipe =
  let
    equal patternError leftType rightType =
      CEqual (Error.Pattern patternError) region leftType rightType

    rvar v =
      A.A region (VarN v)
  in
    case pattern of
      P.Anything ->
        return emptyInfo

      P.Literal lit ->
        do  c <- Literal.constrain env region lit tipe
            return $ emptyInfo { _cons = c }

      P.Var name ->
        do  variable <- mkVar Nothing
            return $ Info
                { _headers = Map.singleton name (rvar variable)
                , _vars = [variable]
                , _cons = equal (Error.PVar name) (VarN variable) tipe
                }

      P.Alias name ptrn ->
        do  variable <- mkVar Nothing
            info <- constrain env ptrn tipe
            return $ info
              { _headers = Map.insert name (rvar variable) (_headers info)
              , _vars = variable : _vars info
              , _cons =
                  equal (Error.PAlias name) (VarN variable) tipe
                  /\ _cons info
              }

      P.Data name patterns ->
        do  let stringName = V.toString name

            (_kind, cvars, args, result) <-
                Env.freshDataScheme env stringName

            fragList <- Monad.zipWithM (constrain env) patterns args
            let info = joinInfos fragList
            return $ info
                { _vars = cvars ++ _vars info
                , _cons =
                    equal (Error.PData stringName) result tipe
                    /\ _cons info
                }

      P.Record fields ->
        do  pairs <-
                mapM (\name -> (,) name <$> mkVar Nothing) fields

            let tenv =
                  Map.fromList (map (second rvar) pairs)

            let unannotatedTenv =
                  Map.map A.drop tenv

            con <- exists $ \t ->
              return (equal Error.PRecord tipe (record unannotatedTenv t))

            return $ Info
                { _headers = tenv
                , _vars = map snd pairs
                , _cons = con
                }
