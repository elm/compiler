module Type.Constrain.Declaration where

import Control.Monad
import Control.Applicative ((<$>))

import qualified Data.Map as Map

import qualified Type.Type as T
import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Environment as Env

import SourceSyntax.Declaration
import qualified SourceSyntax.Location as SL
import qualified SourceSyntax.Literal as SL
import qualified SourceSyntax.Pattern as SP
import qualified SourceSyntax.Expression as SE
import qualified SourceSyntax.Type as ST

{-- testing section --}
import SourceSyntax.PrettyPrint
import SourceSyntax.Module
import Text.PrettyPrint as P
import Parse.Parser (parseProgram)
import Type.Solve (solve)
import qualified Type.State as TS
import Control.Monad.State

test filePath = do
  src <- readFile filePath
  case parseProgram src of
    Left err -> error $ "Parse error at " ++ show err
    Right (Module _ _ _ decls) -> do
      env <- Env.initialEnvironment
      var <- T.flexibleVar
      constraint <- TcExpr.constrain env (toExpr decls) (T.VarN var)
      print =<< T.extraPretty constraint
      (env,_,_,errors) <- execStateT (solve constraint) TS.initialState
      forM (Map.toList env) $ \(n,t) -> do
          pt <- T.extraPretty t
          print $ P.text n <+> P.text ":" <+> pt
      if null errors then return () else do
          putStrLn "\n"
          mapM_ print =<< sequence errors

toExpr decls =
  SL.none $ SE.Let (concatMap toDefs decls) (SL.none $ SE.Literal (SL.IntNum 0))

toDefs :: Declaration t v -> [SE.Def t v]
toDefs decl =
  case decl of
    Definition def -> [def]

    Datatype name tvars constructors -> map toAnnotation constructors
      where
        toAnnotation (ctor, tipes) =
            SE.TypeAnnotation ctor $
                foldr ST.Lambda (ST.Data name $ map ST.Var tvars) tipes

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    TypeAlias _ _ _ -> []

    ImportEvent _ expr@(SL.L a b _) name tipe ->
        [ SE.TypeAnnotation name tipe
        , SE.Def (SP.PVar name) (SL.L a b $ SE.App (SL.L a b $ SE.Var "constant") expr) ]

    ExportEvent _ name tipe ->
        [ SE.TypeAnnotation name tipe ]

    -- no constraints are needed for fixity declarations
    Fixity _ _ _ -> []

toEqualityDict :: Declaration t v -> Maybe (String, IO T.Type)
toEqualityDict decl =
  case decl of
    Definition _ -> Nothing
    Datatype _ _ _ -> Nothing
    ImportEvent _ _ _ _ -> Nothing
    ExportEvent _ _ _ -> Nothing
    Fixity _ _ _ -> Nothing
    TypeAlias alias tvars tipe -> Just $ (,) alias $ do
        pairs <- forM tvars $ \tname -> (,) tname <$> T.namedVar tname
        TcExpr.instantiateTypeWithContext tipe (Map.fromList pairs)
