{-# OPTIONS_GHC -Wall #-}
module Validate (module') where

import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import AST.Expression.General as Expr
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Declaration as D
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Package as Package
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- MODULES


module' :: Module.Source -> Result.Result wrn Error.Error Module.Valid
module' (Module.Module name path info) =
  let
    (ModuleName.Canonical pkgName _) =
      name

    (Module.Source tag settings docs exports imports decls) =
      info
  in
    do  validEffects <- validateModuleDecl tag settings
        validDecls <- declarations decls

        return $ Module.Module name path $
          Module.Valid docs exports (addDefaults pkgName imports) validDecls validEffects


-- determine if default imports should be added, only elm-lang/core is exempt
addDefaults
  :: Package.Name
  -> [Module.UserImport]
  -> ([Module.DefaultImport], [Module.UserImport])
addDefaults pkgName imports =
  flip (,) imports $
    if pkgName == Package.coreName then
      []

    else
      Imports.defaults


validateModuleDecl
  :: Module.SourceTag
  -> Module.SourceSettings
  -> Result.Result wrn Error.Error Module.ValidEffects
validateModuleDecl tag settings =
  case tag of
    Module.SrcNormal ->
      do  noSettings Error.SettingsOnNormalModule settings
          return Module.ValidNormal

    Module.SrcForeign ->
      do  noSettings Error.SettingsOnForeignModule settings
          return Module.ValidForeign

    Module.SrcEffect ->
      gatherEffectSettings settings


noSettings
  :: Error.Error
  -> Module.SourceSettings
  -> Result.Result wrn Error.Error ()
noSettings errorMsg (A.A region settings) =
  case settings of
    [] ->
      Result.ok ()

    _ : _ ->
      Result.throw region errorMsg


gatherEffectSettings
  :: Module.SourceSettings
  -> Result.Result wrn Error.Error Module.ValidEffects
gatherEffectSettings (A.A _ pairs) =
  uncurry Module.ValidEffect <$>
    gatherEffectSettingsHelp (Nothing, Nothing) pairs


type Effects =
  (Maybe (A.Located String), Maybe (A.Located String))


gatherEffectSettingsHelp
  :: Effects
  -> [(A.Located String, A.Located String)]
  -> Result.Result wrn Error.Error Effects
gatherEffectSettingsHelp effects@(maybeCmd, maybeSub) pairs =
  case pairs of
    [] ->
      Result.ok effects

    (A.A region name, effect) : rest ->
      case name of
        "command" ->
          case maybeCmd of
            Nothing ->
              gatherEffectSettingsHelp (Just effect, maybeSub) rest

            Just _ ->
              Result.throw region (Error.DuplicateSettingOnEffectModule name)

        "subscription" ->
          case maybeSub of
            Nothing ->
              gatherEffectSettingsHelp (maybeCmd, Just effect) rest

            Just _ ->
              Result.throw region (Error.DuplicateSettingOnEffectModule name)

        _ ->
          Result.throw region (Error.BadSettingOnEffectModule name)



-- COLLAPSE COMMENTS


type Commented a =
  A.Annotated (R.Region, Maybe (A.Located String)) a


collapseComments
  :: [D.CommentOr (A.Located a)]
  -> Result.Result wrn Error.Error [Commented a]
collapseComments listWithComments =
  case listWithComments of
    [] ->
      Result.ok []

    D.Comment msg : D.Whatever (A.A region a) : rest ->
      let
        entry =
          A.A (region, Just msg) a
      in
        fmap (entry:) (collapseComments rest)

    D.Comment (A.A region _) : D.Comment _ : _ ->
      Result.throw region Error.CommentOnComment

    [D.Comment (A.A region _)] ->
      Result.throw region Error.CommentOnNothing

    D.Whatever (A.A region a) : rest ->
      let
        entry =
          A.A (region, Nothing) a
      in
        fmap (entry:) (collapseComments rest)



-- VALIDATE STRUCTURED SOURCE


declarations :: [D.Source] -> Result.Result wrn Error.Error [D.Valid]
declarations sourceDecls =
  do  decls <-
        collapseComments sourceDecls

      validDecls <-
        validateDecls decls

      F.traverse_ id
        (declDuplicates validDecls : map findBadTypeVars validDecls)

      return validDecls



-- VALIDATE DECLARATIONS


validateDecls
  :: [Commented D.Source']
  -> Result.Result wrn Error.Error [D.Valid]
validateDecls commentedDecls =
  case commentedDecls of
    [] ->
      Result.ok []

    A.A (region, maybeComment) decl : rest ->
      let
        addRest validDecl =
          let
            entry =
              A.A (region, fmap A.drop maybeComment) validDecl
          in
            fmap (entry:) (validateDecls rest)
      in
        case decl of
          D.Union name tvars ctors ->
            addRest (D.Union name tvars ctors)

          D.Alias name tvars alias ->
            addRest (D.Alias name tvars alias)

          D.Fixity assoc prec op ->
            addRest (D.Fixity assoc prec op)

          D.Def def ->
            validateDeclsHelp maybeComment def rest


validateDeclsHelp
  :: Maybe (A.Located String)
  -> Source.Def
  -> [Commented D.Source']
  -> Result.Result wrn Error.Error [D.Valid]
validateDeclsHelp maybeComment (A.A region def) decls =
  let
    ann =
      (region, fmap A.drop maybeComment)

    makeDef pat expr maybeType =
      D.Def (Valid.Definition pat expr maybeType)
  in
    case def of
      Source.Definition pat expr ->
        (:)
          <$> validateDef makeDef ann pat expr Nothing
          <*> validateDecls decls

      Source.Annotation name tipe ->
        case decls of
          A.A (_, Just (A.A commentRegion _)) _ : _ ->
            Result.throw commentRegion (Error.CommentAfterAnnotation name)

          A.A _ (D.Def (A.A _ (Source.Definition pat expr))) : rest
           | Pattern.isVar name pat ->
              (:)
                <$> validateDef makeDef ann pat expr (Just tipe)
                <*> validateDecls rest

          _ ->
            Result.throw region (Error.TypeWithoutDefinition name)



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

    A.A region (Source.Definition pat expr) : rest ->
      (:)
        <$> fmap A.drop (validateDef Valid.Definition region pat expr Nothing)
        <*> definitionsHelp rest

    A.A r1 (Source.Annotation name tipe) : rest ->
      case rest of
        A.A r2 (Source.Definition pat expr) : rest'
          | Pattern.isVar name pat ->
              (:)
                <$> fmap A.drop (validateDef Valid.Definition (R.merge r1 r2) pat expr (Just tipe))
                <*> definitionsHelp rest'

        _ ->
          Result.throw r1 (Error.TypeWithoutDefinition name)


validateDef
  :: (Pattern.Raw -> Valid.Expr -> Maybe Type.Raw -> def)
  -> annotation
  -> Pattern.Raw
  -> Source.Expr
  -> Maybe Type.Raw
  -> Result.Result wrn Error.Error (A.Annotated annotation def)
validateDef makeDef annotation pat expr maybeType =
  do  validExpr <- expression expr
      validateDefPattern pat validExpr
      return $ A.A annotation (makeDef pat validExpr maybeType)


validateDefPattern :: Pattern.Raw -> Valid.Expr -> Result.Result wrn Error.Error ()
validateDefPattern pattern body =
  case fst (Expr.collectLambdas body) of
    [] ->
        return ()

    args ->
        case pattern of
          A.A _ (Pattern.Var _) ->
              return ()

          _ ->
              let
                (A.A start _) = pattern
                (A.A end _) = last args
              in
                Result.throw (R.merge start end) (Error.BadFunctionName (length args))



-- VALIDATE EXPRESSIONS


expression :: Source.Expr -> Result.Result wrn Error.Error Valid.Expr
expression (A.A ann sourceExpression) =
  A.A ann <$>
  case sourceExpression of
    Var x ->
        return (Var x)

    Lambda pattern body ->
        Lambda
            <$> validatePattern pattern
            <*> expression body

    Binop op leftExpr rightExpr ->
        Binop op
          <$> expression leftExpr
          <*> expression rightExpr

    Case e branches ->
        Case
          <$> expression e
          <*> T.traverse (\(p,b) -> (,) <$> validatePattern p <*> expression b) branches

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

    If branches finally ->
        If
          <$> T.traverse both branches
          <*> expression finally

    Access record field ->
        Access
          <$> expression record
          <*> return field

    Update record fields ->
        Update
          <$> expression record
          <*> T.traverse second fields

    Record fields ->
        let
          checkDups seenFields (field,_) =
              if Set.member field seenFields then
                  Result.throw ann (Error.DuplicateFieldName field)

              else
                  return (Set.insert field seenFields)
        in
          do  _ <- foldM checkDups Set.empty fields
              Record <$> T.traverse second fields

    Let defs body ->
        Let
          <$> definitions defs
          <*> expression body

    GLShader uid src gltipe ->
        return (GLShader uid src gltipe)


second :: (a, Source.Expr) -> Result.Result wrn Error.Error (a, Valid.Expr)
second (value, expr) =
    (,) value <$> expression expr


both
  :: (Source.Expr, Source.Expr)
  -> Result.Result wrn Error.Error (Valid.Expr, Valid.Expr)
both (expr1, expr2) =
    (,) <$> expression expr1 <*> expression expr2



-- VALIDATE PATTERNS


validatePattern :: Pattern.Raw -> Result.Result wrn Error.Error Pattern.Raw
validatePattern pattern =
  do  detectDuplicates Error.BadPattern (Pattern.boundVars pattern)
      return pattern



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


defDuplicates :: [Pattern.Raw] -> Result.Result wrn Error.Error ()
defDuplicates patterns =
  concatMap Pattern.boundVars patterns
    |> detectDuplicates Error.DuplicateDefinition


declDuplicates :: [D.Valid] -> Result.Result wrn Error.Error ()
declDuplicates decls =
  let
    (valueLists, typeLists) =
      unzip (map extractValues decls)
  in
    (\_ _ -> ())
      <$> detectDuplicates Error.DuplicateValueDeclaration (concat valueLists)
      <*> detectDuplicates Error.DuplicateTypeDeclaration (concat typeLists)


extractValues :: D.Valid -> ([A.Located String], [A.Located String])
extractValues (A.A (region, _) decl) =
  case decl of
    D.Def (Valid.Definition pattern _ _) ->
        ( Pattern.boundVars pattern
        , []
        )

    D.Union name _ ctors ->
        ( map (A.A region . fst) ctors
        , [A.A region name]
        )

    D.Alias name _ (A.A _ (Type.RRecord _ _)) ->
        ( [A.A region name]
        , [A.A region name]
        )

    D.Alias name _ _ ->
        ( []
        , [A.A region name]
        )

    D.Fixity _ _ _ ->
        ( []
        , []
        )



-- UNBOUND TYPE VARIABLES


findBadTypeVars :: D.Valid -> Result.Result wrn Error.Error ()
findBadTypeVars (A.A (region,_) decl) =
  case decl of
    D.Def _ ->
        return ()

    D.Union name boundVars ctors ->
        case diff boundVars (concatMap freeVars (concatMap snd ctors)) of
          (_, []) ->
              return ()

          (_, unbound) ->
              Result.throw region
                (Error.UnboundTypeVarsInUnion name boundVars unbound)

    D.Alias name boundVars tipe ->
        case diff boundVars (freeVars tipe) of
          ([], []) ->
              return ()

          ([], unbound) ->
              Result.throw region
                (Error.UnboundTypeVarsInAlias name boundVars unbound)

          (unused, []) ->
              Result.throw region
                (Error.UnusedTypeVarsInAlias name boundVars unused)

          (unused, unbound) ->
              Result.throw region
                (Error.MessyTypeVarsInAlias name boundVars unused unbound)

    D.Fixity _ _ _ ->
        return ()


diff :: [String] -> [A.Located String] -> ([String], [String])
diff left right =
  let
    leftSet =
      Set.fromList left

    rightSet =
      Set.fromList (map A.drop right)
  in
    ( Set.toList (Set.difference leftSet rightSet)
    , Set.toList (Set.difference rightSet leftSet)
    )



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

    Type.REffects _ ->
      []

