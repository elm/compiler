{-# OPTIONS_GHC -Wall #-}
module Transform.Declaration (combineAnnotations, toExpr) where

import Control.Applicative ((<$>))
import Data.List (stripPrefix)
import Data.Char (isSpace)

import qualified AST.Annotation as A
import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as Var

import qualified Transform.Expression as Expr
import qualified Transform.Definition as Def

extractDocComment :: String -> Maybe String
extractDocComment doc =
  do prefixLess <- stripPrefix "{-|" doc
     suffixLess <- stripSuffix "-}" prefixLess
     return $ dropWhile isSpace suffixLess
  where stripSuffix suf str =
          let (part1, part2) = splitAt (length str - length suf) str
          in case part2 == suf of
            False -> Nothing
            True -> Just part1

-- Attach document comments to following definitions.
-- Two doccomments in a row is an error, doccomment at the end of the file
-- is an error. Notice that any resulting declaration can't be a DocComment
attachComments :: [D.ValidDecl'] -> Either String [D.ValidDecl]
attachComments decls =
  case decls of
    D.DocComment _ : D.DocComment _ : _ ->
      Left "Two consequent document comments found!"
    D.DocComment doc : next : rest ->
      (:) (D.Declaration next (Just doc)) <$> attachComments rest
    D.DocComment _ : [] ->
      Left "Document comment at the end of the file!"
    next : rest ->
      (:) (D.Declaration next Nothing) <$> attachComments rest
    [] -> return []

combineAnnotations :: [D.SourceDecl] -> Either String [D.ValidDecl]
combineAnnotations decls =
  do step1 <- attachTypeSignatures decls
     attachComments step1

attachTypeSignatures :: [D.SourceDecl] -> Either String [D.ValidDecl']
attachTypeSignatures = go
    where
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."

      exprCombineAnnotations = Expr.crawlLet Def.combineAnnotations

      go decls =
          case decls of
            -- simple cases, pass them through with no changes
            [] -> return []

            D.Datatype name tvars ctors : rest ->
                (:) (D.Datatype name tvars ctors) <$> go rest

            D.TypeAlias name tvars alias : rest ->
                (:) (D.TypeAlias name tvars alias) <$> go rest

            D.Fixity assoc prec op : rest ->
                (:) (D.Fixity assoc prec op) <$> go rest

            -- remove all multiline comments which are not doccomments
            D.DocComment comment : rest ->
              case extractDocComment comment of
                Just doc -> (:) (D.DocComment doc) <$> go rest
                Nothing -> go rest

            -- combine definitions
            D.Definition def : defRest ->
                case def of
                  Source.Definition pat expr ->
                      do expr' <- exprCombineAnnotations expr
                         let def' = Valid.Definition pat expr' Nothing
                         (:) (D.Definition def') <$> go defRest

                  Source.TypeAnnotation name tipe ->
                      case defRest of
                        D.Definition (Source.Definition pat@(P.Var name') expr) : rest
                            | name == name' ->
                                do expr' <- exprCombineAnnotations expr
                                   let def' = Valid.Definition pat expr' (Just tipe)
                                   (:) (D.Definition def') <$> go rest

                        _ -> Left (msg name)

            -- combine ports
            D.Port port : portRest ->
                case port of
                  D.PPDef name _ -> Left (msg name)
                  D.PPAnnotation name tipe ->
                      case portRest of
                        D.Port (D.PPDef name' expr) : rest | name == name' ->
                            do expr' <- exprCombineAnnotations expr
                               (:) (D.Port (D.Out name expr' tipe)) <$> go rest

                        _ -> (:) (D.Port (D.In name tipe)) <$> go portRest


toExpr :: String -> [D.CanonicalDecl] -> [Canonical.Def]
toExpr moduleName = concatMap (toDefs moduleName)

toDefs :: String -> D.CanonicalDecl -> [Canonical.Def]
toDefs moduleName decl =
  let typeVar = Var.Canonical (Var.Module moduleName) in
  case D.declBody decl of
    D.Definition def -> [def]

    D.Datatype name tvars constructors -> concatMap toDefs' constructors
      where
        toDefs' (ctor, tipes) =
            let vars = take (length tipes) arguments
                tbody = T.App (T.Type (typeVar name)) (map T.Var tvars)
                body = A.none . E.Data ctor $ map (A.none . E.localVar) vars
            in  [ definition ctor (buildFunction body vars) (foldr T.Lambda tbody tipes) ]

    D.TypeAlias name _ tipe@(T.Record fields ext) ->
        [ definition name (buildFunction record vars) (foldr T.Lambda result args) ]
      where
        result = T.Aliased (typeVar name) tipe

        args = map snd fields ++ maybe [] (:[]) ext

        var = A.none . E.localVar
        vars = take (length args) arguments

        efields = zip (map fst fields) (map var vars)
        record = case ext of
                   Nothing -> A.none $ E.Record efields
                   Just _ -> foldl (\r (f,v) -> A.none $ E.Insert r f v) (var $ last vars) efields

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    D.TypeAlias{} -> []

    D.Port port ->
        case port of
          D.Out name expr@(A.A s _) tipe ->
              [ definition name (A.A s $ E.PortOut name tipe expr) tipe ]
          D.In name tipe ->
              [ definition name (A.none $ E.PortIn name tipe) tipe ]

    -- no constraints are needed for fixity declarations
    D.Fixity{} -> []

    -- doccomments shouldn't be in a canonical declaration
    D.DocComment{} -> error "Error: DocComment in canonical declaration"


arguments :: [String]
arguments = map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show (n :: Int)) [1..]

buildFunction :: Canonical.Expr -> [String] -> Canonical.Expr
buildFunction body@(A.A s _) vars =
    foldr (\p e -> A.A s (E.Lambda p e)) body (map P.Var vars)

definition :: String -> Canonical.Expr -> T.CanonicalType -> Canonical.Def
definition name expr tipe = Canonical.Definition (P.Var name) expr (Just tipe)
