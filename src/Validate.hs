{-# OPTIONS_GHC -Wall #-}
module Validate (declarations) where

import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import AST.Expression.General
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Declaration as D
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result


-- VALIDATE DECLARATIONS

declarations
    :: [D.SourceDecl]
    -> Result.Result wrn Error.Error [D.ValidDecl]
declarations sourceDecls =
  do  validDecls <- validateDecls sourceDecls
      (\_ _ -> validDecls)
        <$> declDuplicates validDecls
        <*> T.traverse checkDecl validDecls


validateDecls
    :: [D.SourceDecl]
    -> Result.Result wrn Error.Error [D.ValidDecl]
validateDecls sourceDecls =
  case sourceDecls of
    [] ->
        return []

    decl : decls ->
        validateDeclsHelp decl decls


validateDeclsHelp
    :: D.SourceDecl
    -> [D.SourceDecl]
    -> Result.Result wrn Error.Error [D.ValidDecl]
validateDeclsHelp (A.A region decl) decls =
  let addRest validDecl =
        (:) (A.A region validDecl) <$> validateDecls decls
  in
  case decl of
    D.Datatype name tvars ctors ->
        addRest (D.Datatype name tvars ctors)

    D.TypeAlias name tvars alias ->
        addRest (D.TypeAlias name tvars alias)

    D.Fixity assoc prec op ->
        addRest (D.Fixity assoc prec op)

    D.Definition def ->
        defHelp def decls

    D.Port port ->
        portHelp region port decls


-- VALIDATE DEFINITIONS IN DECLARATIONS

defHelp
    :: Source.Def
    -> [D.SourceDecl]
    -> Result.Result wrn Error.Error [D.ValidDecl]
defHelp (A.A region def) decls =
  let addRest def' rest =
        (:) (A.A region (D.Definition def')) <$> validateDecls rest
  in
  case def of
    Source.Definition pat expr ->
        do  expr' <- Validate.expression expr
            let def' = Valid.Definition pat expr' Nothing
            addRest def' decls

    Source.TypeAnnotation name tipe ->
        case decls of
          A.A _ (D.Definition (A.A _ (Source.Definition pat@(A.A _ (Pattern.Var name')) expr))) : rest
              | name == name' ->
                  do  expr' <- Validate.expression expr
                      let def' = Valid.Definition pat expr' (Just tipe)
                      addRest def' rest

          _ ->
              Result.throw region (Error.TypeWithoutDefinition name)


-- VALIDATE PORTS IN DECLARATIONS

portHelp
    :: R.Region
    -> D.SourcePort
    -> [D.SourceDecl]
    -> Result.Result wrn Error.Error [D.ValidDecl]
portHelp region port decls =
  let addRest port' rest =
        (:) (A.A region (D.Port port')) <$> validateDecls rest
  in
  case port of
    D.PortDefinition name _ ->
        Result.throw region (Error.PortWithoutAnnotation name)

    D.PortAnnotation name tipe ->
        case decls of
          A.A _ (D.Port (D.PortDefinition name' expr)) : rest
              | name == name' ->
                  do  expr' <- Validate.expression expr
                      let port' = D.Out name expr' tipe
                      addRest port' rest

          _ ->
              addRest (D.In name tipe) decls


-- VALIDATE DEFINITIONS

definitions :: [Source.Def] -> Result.Result wrn Error.Error [Valid.Def]
definitions sourceDefs =
  do  validDefs <- definitionsHelp sourceDefs
      let patterns = map (\(Valid.Definition p _ _) -> p) validDefs
      defDuplicates patterns
      return validDefs


definitionsHelp :: [Source.Def] -> Result.Result wrn Error.Error [Valid.Def]
definitionsHelp sourceDefs =
  case sourceDefs of
    [] ->
        return []

    A.A _ (Source.Definition pat expr) : rest ->
        do  expr' <- expression expr
            let def = Valid.Definition pat expr' Nothing
            (:) def <$> definitionsHelp rest

    A.A region (Source.TypeAnnotation name tipe) : rest ->
        case rest of
          A.A _ (Source.Definition pat@(A.A _ (Pattern.Var name')) expr) : rest'
              | name == name' ->
                  do  expr' <- expression expr
                      let def = Valid.Definition pat expr' (Just tipe)
                      (:) def <$> definitionsHelp rest'

          _ ->
              Result.throw region (Error.TypeWithoutDefinition name)


-- VALIDATE EXPRESSIONS

expression :: Source.Expr -> Result.Result wrn Error.Error Valid.Expr
expression (A.A ann sourceExpression) =
  A.A ann <$>
  case sourceExpression of
    Var x ->
        return (Var x)

    Lambda pattern body ->
        Lambda pattern <$> expression body

    Binop op leftExpr rightExpr ->
        Binop op
          <$> expression leftExpr
          <*> expression rightExpr

    Case e branches ->
        Case
          <$> expression e
          <*> T.traverse second branches

    Data name args ->
        Data name <$> T.traverse expression args

    Literal lit ->
        return (Literal lit)

    Range lowExpr highExpr ->
        Range
          <$> expression lowExpr
          <*> expression highExpr

    ExplicitList expressions ->
        ExplicitList
          <$> T.traverse expression expressions

    App funcExpr argExpr ->
        App
          <$> expression funcExpr
          <*> expression argExpr

    MultiIf branches ->
        MultiIf <$> T.traverse both branches

    Access record field ->
        Access
          <$> expression record
          <*> return field

    Remove record field ->
        Remove
          <$> expression record
          <*> return field

    Insert record field expr ->
        Insert
          <$> expression record
          <*> return field
          <*> expression expr

    Modify record fields ->
        Modify
          <$> expression record
          <*> T.traverse second fields

    Record fields ->
        Record
          <$> T.traverse second fields

    Let defs body ->
        Let
          <$> definitions defs
          <*> expression body

    GLShader uid src gltipe ->
        return (GLShader uid src gltipe)

    Port impl ->
        Port <$>
          case impl of
            In name tipe ->
                return (In name tipe)

            Out name expr tipe ->
                do  expr' <- expression expr
                    return (Out name expr' tipe)

            Task name expr tipe ->
                do  expr' <- expression expr
                    return (Task name expr' tipe)


second :: (a, Source.Expr) -> Result.Result wrn Error.Error (a, Valid.Expr)
second (value, expr) =
    (,) value <$> expression expr


both
    :: (Source.Expr, Source.Expr)
    -> Result.Result wrn Error.Error (Valid.Expr, Valid.Expr)
both (expr1, expr2) =
    (,) <$> expression expr1 <*> expression expr2


-- DETECT DUPLICATES

detectDuplicates
    :: (String -> Error.Error)
    -> [A.Located String]
    -> Result.Result wrn Error.Error ()
detectDuplicates tag names =
  let add (A.A region name) dict =
          Map.insertWith (++) name [region] dict

      makeGroups pairs =
          Map.toList (foldr add Map.empty pairs)

      check (name, regions) =
        case regions of
          _ : region : _ ->
              Result.throw region (tag name)

          _ ->
              return ()
  in
      F.traverse_ check (makeGroups names)


defDuplicates
    :: [Pattern.RawPattern]
    -> Result.Result wrn Error.Error ()
defDuplicates patterns =
  concatMap Pattern.boundVars patterns
    |> detectDuplicates Error.DuplicateDefinition


declDuplicates :: [D.ValidDecl] -> Result.Result wrn Error.Error ()
declDuplicates decls =
  let (valueLists, typeLists) = unzip (map extractValues decls)
  in
      (\_ _ -> ())
        <$> detectDuplicates Error.DuplicateValueDeclaration (concat valueLists)
        <*> detectDuplicates Error.DuplicateTypeDeclaration (concat typeLists)


extractValues :: D.ValidDecl -> ([A.Located String], [A.Located String])
extractValues (A.A region decl) =
  case decl of
    D.Definition (Valid.Definition pattern _ _) ->
        ( Pattern.boundVars pattern
        , []
        )

    D.Datatype name _ ctors ->
        ( map (A.A region . fst) ctors
        , [A.A region name]
        )

    D.TypeAlias name _ (A.A _ (Type.RRecord _ _)) ->
        ( [A.A region name]
        , [A.A region name]
        )

    D.TypeAlias name _ _ ->
        ( []
        , [A.A region name]
        )

    D.Port port ->
        ( [A.A region (D.validPortName port)]
        , []
        )

    D.Fixity _ _ _ ->
        ( []
        , []
        )


-- UNBOUND TYPE VARIABLES

checkDecl :: D.ValidDecl -> Result.Result wrn Error.Error ()
checkDecl (A.A _ decl) =
  case decl of
    D.Definition _ ->
        return ()

    D.Datatype name boundVars ctors ->
        F.traverse_ (checkTypeVars name boundVars) (concatMap snd ctors)

    D.TypeAlias name boundVars tipe ->
        checkTypeVars name boundVars tipe

    D.Port _ ->
        return ()

    D.Fixity _ _ _ ->
        return ()


checkTypeVars :: String -> [String] -> Type.Raw -> Result.Result wrn Error.Error ()
checkTypeVars name boundVars tipe =
  let bounds = Set.fromList boundVars

      check (A.A region free) =
          if Set.member free bounds
            then return ()
            else Result.throw region (Error.UnboundTypeVar name free)
  in
      F.traverse_ check (freeVars tipe)


freeVars :: Type.Raw -> [A.Located String]
freeVars (A.A region tipe) =
    case tipe of
      Type.RLambda t1 t2 ->
          freeVars t1 ++ freeVars t2

      Type.RVar x ->
          [A.A region x]

      Type.RType _ ->
          []

      Type.RApp t ts ->
          concatMap freeVars (t:ts)

      Type.RRecord fields ext ->
          maybe [] freeVars ext
          ++ concatMap (freeVars . snd) fields

