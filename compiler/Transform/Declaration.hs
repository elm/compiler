{-# OPTIONS_GHC -Wall #-}
module Transform.Declaration (validate, toExpr) where

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

validate :: [D.SourceDecl] -> Either String [D.ValidDecl]
validate decls =
    combineAnnotations =<< attachComments decls

(<:>) :: (Functor f) => a -> f [a] -> f [a]
x <:> f = (:) x <$> f

attachComments :: [D.SourceDecl] -> Either String [D.AnnotatedDecl D.SourceDecl']
attachComments srcDecls =
    go Nothing srcDecls
  where
    go maybeComment decls =
        case decls of
          D.DocComment doc : rest ->
              case maybeComment of
                Just _ -> Left msgTwo
                Nothing -> go (extractDocComment doc) rest

          D.Decl decl : rest ->
              A.A maybeComment decl <:> go Nothing rest

          [] -> maybe (return []) (const (Left msgEnd)) maybeComment

    extractDocComment :: String -> Maybe String
    extractDocComment doc =
        do prefixLess <- stripPrefix "{-|" doc
           suffixLess <- stripSuffix "-}" prefixLess
           return $ dropWhile isSpace suffixLess
        where
          stripSuffix suf str =
              let (part1, part2) = splitAt (length str - length suf) str
              in  if part2 == suf
                    then Nothing
                    else Just part1

    msgTwo =
        "Found two document comments in a row! All document comments\
        \ must be associated with a top level declaration.\
        \ Maybe you commented out a declaration?"

    msgEnd =
        "Found document comment at the end of the file! All document\
        \ comments must be associated with a top level declaration.\
        \ Maybe you commented out a declaration?"

combineAnnotations :: [D.AnnotatedDecl D.SourceDecl'] -> Either String [D.ValidDecl]
combineAnnotations = go
  where
    msg x = "Syntax Error: The type annotation for '" ++ x ++
            "' must be directly above its definition."

    exprCombineAnnotations = Expr.crawlLet Def.combineAnnotations

    go decls =
      case decls of
        [] -> return []
        (A.A comment decl) : rest ->
          let goAfter d = A.A comment d <:> go rest in
          case decl of
            -- actually combine type annotations
            D.Definition def -> combineDefinitions def comment rest

            D.Port port -> combinePorts port comment rest

            -- simple pass through
            D.Datatype name tvars ctors ->
                goAfter (D.Datatype name tvars ctors)

            D.TypeAlias name tvars alias ->
                goAfter (D.TypeAlias name tvars alias)

            D.Fixity assoc prec op ->
                goAfter (D.Fixity assoc prec op)

    noComment :: Maybe String -> Either String a -> Either String a
    noComment doc value =
        case doc of
          Nothing -> value
          Just _ -> Left "A document comment can not come after a type annotation!"

    combineDefinitions def comment rest =
      let continue decl remaining = A.A comment decl <:> go remaining in
      case def of
        Source.Definition pat expr ->
            do expr' <- exprCombineAnnotations expr
               let def' = Valid.Definition pat expr' Nothing
               continue (D.Definition def') rest

        Source.TypeAnnotation name tipe ->
            case rest of
              [] -> Left (msg name)
              (A.A doc decl) : rest2 ->
                  noComment doc $
                  case decl of
                    D.Definition (Source.Definition pat@(P.Var name') expr)
                        | name == name' ->
                            do expr' <- exprCombineAnnotations expr
                               let def' = Valid.Definition pat expr' (Just tipe)
                               continue (D.Definition def') rest2

                    _ -> Left (msg name)

    combinePorts port comment rest =
      let continue decl remaining = A.A comment decl <:> go remaining in
      case port of
        D.PPDef name _ -> Left (msg name)
        D.PPAnnotation name tipe ->
            case rest of
              [] -> continue (D.Port (D.In name tipe)) []

              (A.A doc decl) : rest2 ->
                  noComment doc $
                  case decl of
                    D.Port (D.PPDef name' expr) | name == name' ->
                        do expr' <- exprCombineAnnotations expr
                           continue (D.Port (D.Out name expr' tipe)) rest2

                    _ -> continue (D.Port (D.In name tipe)) rest


toExpr :: String -> [D.CanonicalDecl] -> [Canonical.Def]
toExpr moduleName = concatMap (toDefs moduleName)

toDefs :: String -> D.CanonicalDecl -> [Canonical.Def]
toDefs moduleName decl =
  let typeVar = Var.Canonical (Var.Module moduleName) in
  case A.value decl of
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
    D.TypeAlias _ _ _ -> []

    D.Port port ->
        case port of
          D.Out name expr@(A.A s _) tipe ->
              [ definition name (A.A s $ E.PortOut name tipe expr) tipe ]
          D.In name tipe ->
              [ definition name (A.none $ E.PortIn name tipe) tipe ]

    -- no constraints are needed for fixity declarations
    D.Fixity _ _ _ -> []

arguments :: [String]
arguments = map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show (n :: Int)) [1..]

buildFunction :: Canonical.Expr -> [String] -> Canonical.Expr
buildFunction body@(A.A s _) vars =
    foldr (\p e -> A.A s (E.Lambda p e)) body (map P.Var vars)

definition :: String -> Canonical.Expr -> T.CanonicalType -> Canonical.Def
definition name expr tipe = Canonical.Definition (P.Var name) expr (Just tipe)
