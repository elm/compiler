{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Validate (Result, validate) where

import Control.Monad (foldM_, when)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Effects as Effects
import qualified AST.Expression.Source as Src
import qualified AST.Declaration as D
import qualified AST.Literal as L
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Package as Package
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



type Result warning a =
  Result.Result () warning Error.Error a



-- MODULES


validate :: Module.Source -> Result w Module.Valid
validate (Module.Module name info) =
  let
    (ModuleName.Canonical pkgName _) =
      name

    (Module.Source tag settings docs exports imports decls) =
      info
  in
    do  (ValidStuff ports structure) <- validateDecls decls

        validEffects <- validateEffects tag settings ports (D._defs structure)

        return $ Module.Module name $
          Module.Valid docs exports (addDefaults pkgName imports) structure validEffects



-- IMPORTS


addDefaults
  :: Package.Name
  -> [Module.UserImport]
  -> ([Module.DefaultImport], [Module.UserImport])
addDefaults pkgName imports =
  flip (,) imports $
    if pkgName == Package.core then
      []

    else
      Imports.defaults



-- EFFECTS


validateEffects
  :: Module.SourceTag
  -> Module.SourceSettings
  -> [A.Commented Effects.PortRaw]
  -> [A.Commented Src.ValidDef]
  -> Result w Effects.Raw
validateEffects tag settings@(A.A _ pairs) ports validDefs =
  case tag of
    Module.Normal ->
      do  noSettings Error.SettingsOnNormalModule settings
          noPorts ports
          return Effects.None

    Module.Port _ ->
      do  noSettings Error.SettingsOnPortModule settings
          return (Effects.Port ports)

    Module.Effect tagRegion ->
      let
        collectSettings (A.A region setting, userValue) dict =
          Map.insertWith (++) setting [(region, userValue)] dict

        settingsDict =
          foldr collectSettings Map.empty pairs
      in
        do  noPorts ports
            managerType <- toManagerType tagRegion settingsDict
            (r0, r1, r2) <- checkManager tagRegion managerType validDefs
            return (Effects.Manager () (Effects.Info tagRegion r0 r1 r2 managerType))


noSettings :: Error.Error -> Module.SourceSettings -> Result w ()
noSettings errorMsg (A.A region settings) =
  case settings of
    [] ->
      Result.ok ()

    _ : _ ->
      Result.throw region errorMsg


noPorts :: [A.Commented Effects.PortRaw] -> Result w ()
noPorts ports =
  case ports of
    [] ->
      Result.ok ()

    _ : _ ->
      let
        toError (A.A (region, _) (Effects.PortRaw name _)) =
          A.A region (Error.UnexpectedPort name)
      in
        Result.throwMany (map toError ports)


toManagerType
  :: R.Region
  -> Map.Map Text [(R.Region, A.Located Text)]
  -> Result w Effects.RawManagerType
toManagerType tagRegion settingsDict =
  let
    toErrors name entries =
      map (\entry -> A.A (fst entry) (Error.BadSettingOnEffectModule name)) entries

    errors =
      settingsDict
        |> Map.delete "command"
        |> Map.delete "subscription"
        |> Map.mapWithKey toErrors
        |> Map.elems
        |> concat
  in
    do  when (not (null errors)) (Result.throwMany errors)
        maybeEffects <-
          (,) <$> extractOne "command" settingsDict
              <*> extractOne "subscription" settingsDict

        -- TODO check that cmd and sub types exist?
        case maybeEffects of
          (Nothing, Nothing) ->
            Result.throw tagRegion Error.NoSettingsOnEffectModule

          (Just cmd, Nothing) ->
            return (Effects.CmdManager cmd)

          (Nothing, Just sub) ->
            return (Effects.SubManager sub)

          (Just cmd, Just sub) ->
            return (Effects.FxManager cmd sub)


extractOne
  :: Text
  -> Map.Map Text [(R.Region, A.Located Text)]
  -> Result w (Maybe (A.Located Text))
extractOne name settingsDict =
  case Map.lookup name settingsDict of
    Nothing ->
      return Nothing

    Just [] ->
      error "Empty lists should never be added to the dictionary of effect module settings."

    Just [(_, userType)] ->
      return (Just userType)

    Just ((region, _) : _) ->
      Result.throw region (Error.DuplicateSettingOnEffectModule name)



-- CHECK EFFECT MANAGER


checkManager
  :: R.Region
  -> Effects.RawManagerType
  -> [A.Commented Src.ValidDef]
  -> Result w (R.Region, R.Region, R.Region)
checkManager tagRegion managerType validDefs =
  let
    regionDict =
      Map.fromList (Maybe.mapMaybe getSimpleDefRegion validDefs)
  in
  const (,,)
    <$> requireMaps tagRegion regionDict managerType
    <*> requireRegion tagRegion regionDict "init"
    <*> requireRegion tagRegion regionDict "onEffects"
    <*> requireRegion tagRegion regionDict "onSelfMsg"


getSimpleDefRegion :: A.Commented Src.ValidDef -> Maybe (Text, R.Region)
getSimpleDefRegion decl =
  case decl of
    A.A _ (Src.Def region (A.A _ (Pattern.Var name)) _ _) ->
      Just (name, region)

    _ ->
      Nothing


requireMaps
  :: R.Region
  -> Map.Map Text R.Region
  -> Effects.RawManagerType
  -> Result w ()
requireMaps tagRegion regionDict managerType =
  let
    check name =
      when (Map.notMember name regionDict) $
        Result.throw tagRegion (Error.MissingManagerOnEffectModule name)
  in
  case managerType of
    Effects.CmdManager _ ->
      check "cmdMap"

    Effects.SubManager _ ->
      check "subMap"

    Effects.FxManager _ _ ->
      check "cmdMap" <* check "subMap"


requireRegion :: R.Region -> Map.Map Text R.Region -> Text -> Result w R.Region
requireRegion tagRegion regionDict name =
  case Map.lookup name regionDict of
    Just region ->
      return region

    Nothing ->
      Result.throw tagRegion (Error.MissingManagerOnEffectModule name)



-- COLLAPSE COMMENTS


collapseComments :: [D.CommentOr (A.Located a)] -> Result w [A.Commented a]
collapseComments listWithComments =
  case listWithComments of
    [] ->
      Result.ok []

    D.Comment (A.A _ msg) : D.Whatever (A.A region a) : rest ->
      let
        entry =
          A.A (region, Just msg) a
      in
        fmap (entry:) (collapseComments rest)

    D.Comment (A.A region _) : rest ->
      collapseComments rest
        <* Result.throw region Error.CommentOnNothing


    D.Whatever (A.A region a) : rest ->
      let
        entry =
          A.A (region, Nothing) a
      in
        fmap (entry:) (collapseComments rest)



-- VALIDATE STRUCTURED SOURCE


validateDecls :: [D.Source] -> Result w ValidStuff
validateDecls sourceDecls =
  do  rawDecls <- collapseComments sourceDecls

      validStuff <- validateRawDecls rawDecls

      let (D.Decls _ unions aliases _) = _structure validStuff

      return validStuff
        <* F.traverse_ checkTypeVarsInUnion unions
        <* F.traverse_ checkTypeVarsInAlias aliases
        <* checkDuplicates validStuff



-- VALIDATE DECLARATIONS


data ValidStuff =
  ValidStuff
    { _ports :: [A.Commented Effects.PortRaw]
    , _structure :: D.Valid
    }


validateRawDecls :: [A.Commented D.Raw] -> Result w ValidStuff
validateRawDecls commentedDecls =
  vrdHelp commentedDecls [] (D.Decls [] [] [] [])


vrdHelp
  :: [A.Commented D.Raw]
  -> [A.Commented Effects.PortRaw]
  -> D.Valid
  -> Result w ValidStuff
vrdHelp commentedDecls ports structure =
  case commentedDecls of
    [] ->
      Result.ok (ValidStuff ports structure)

    A.A ann decl : rest ->
      case decl of
        D.Union (D.Type name tvars ctors) ->
          vrdHelp rest ports (D.addUnion (A.A ann (D.Type name tvars ctors)) structure)

        D.Alias (D.Type name tvars alias) ->
          vrdHelp rest ports (D.addAlias (A.A ann (D.Type name tvars alias)) structure)

        D.Fixity fixity ->
          vrdHelp rest ports (D.addInfix fixity structure)

        D.Def (A.A region def) ->
          vrdDefHelp rest (A.A (region, snd ann) def) ports structure

        D.Port name tipe ->
          do  checkDuplicateFieldsInType tipe
              vrdHelp rest (A.A ann (Effects.PortRaw name tipe) : ports) structure


vrdDefHelp
  :: [A.Commented D.Raw]
  -> A.Commented Src.RawDef'
  -> [A.Commented Effects.PortRaw]
  -> D.Valid
  -> Result w ValidStuff
vrdDefHelp remainingDecls (A.A ann def) ports structure =
  let
    addDef validDef (ValidStuff finalPorts struct) =
      ValidStuff finalPorts (D.addDef (A.A ann validDef) struct)
  in
    case def of
      Src.Definition pat expr ->
        addDef
          <$> validateDef (fst ann) pat expr Nothing
          <*> vrdHelp remainingDecls ports structure

      Src.Annotation name tipe ->
        case remainingDecls of
          A.A _ (D.Def (A.A defRegion (Src.Definition pat expr))) : rest
           | Pattern.isVar name pat ->
              addDef
                <$> validateDef (R.merge (fst ann) defRegion) pat expr (Just tipe)
                <*> vrdHelp rest ports structure

          _ ->
            vrdHelp remainingDecls ports structure
              <* Result.throw (fst ann) (Error.TypeWithoutDefinition name)



-- VALIDATE DEFINITIONS


definitions :: [Src.RawDef] -> Result w [Src.ValidDef]
definitions sourceDefs =
  do  validDefs <- definitionsHelp sourceDefs

      validDefs
        |> map Src.getPattern
        |> concatMap Pattern.boundVars
        |> detectDuplicates Error.DuplicateDefinition

      return validDefs


definitionsHelp :: [Src.RawDef] -> Result w [Src.ValidDef]
definitionsHelp sourceDefs =
  case sourceDefs of
    [] ->
      return []

    A.A defRegion (Src.Definition pat expr) : rest ->
      (:)
        <$> validateDef defRegion pat expr Nothing
        <*> definitionsHelp rest

    A.A annRegion (Src.Annotation name tipe) : rest ->
      case rest of
        A.A defRegion (Src.Definition pat expr) : rest'
          | Pattern.isVar name pat ->
              (:)
                <$> validateDef (R.merge annRegion defRegion) pat expr (Just tipe)
                <*> definitionsHelp rest'

        _ ->
          Result.throw annRegion (Error.TypeWithoutDefinition name)


validateDef
  :: R.Region
  -> Pattern.Raw
  -> Src.RawExpr
  -> Maybe Type.Raw
  -> Result w Src.ValidDef
validateDef region pat expr maybeType =
  do  validExpr <- expression expr
      validateDefPattern pat validExpr
      F.traverse_ checkDuplicateFieldsInType maybeType
      return $ Src.Def region pat validExpr maybeType


validateDefPattern :: Pattern.Raw -> Src.ValidExpr -> Result w ()
validateDefPattern pattern body =
  case fst (Src.collectLambdas body) of
    [] ->
        return ()

    args ->
        case pattern of
          A.A _ (Pattern.Var funcName) ->
              checkArguments funcName args

          _ ->
              let
                (A.A start _) = pattern
                (A.A end _) = last args
              in
                Result.throw (R.merge start end) (Error.BadFunctionName (length args))


checkArguments :: Text -> [Pattern.Raw] -> Result w ()
checkArguments funcName args =
  let
    vars =
      concatMap Pattern.boundVars args

    checkDups seenArgs (A.A region arg) =
      if Set.member arg seenArgs then
        Result.throw region (Error.DuplicateArgument funcName arg)

      else
        return (Set.insert arg seenArgs)
  in
    foldM_ checkDups Set.empty vars



-- VALIDATE EXPRESSIONS


expression :: Src.RawExpr -> Result w Src.ValidExpr
expression (A.A ann sourceExpression) =
  A.A ann <$>
  case sourceExpression of
    Src.Var (Var.Raw "True") ->
        return (Src.Literal (L.Boolean True))

    Src.Var (Var.Raw "False") ->
        return (Src.Literal (L.Boolean False))

    Src.Var x ->
        return (Src.Var x)

    Src.Lambda pattern body ->
        Src.Lambda
          <$> validatePattern pattern
          <*> expression body

    Src.Binop ops lastExpr ->
        Src.Binop
          <$> traverse (\(expr,op) -> (,) <$> expression expr <*> pure op) ops
          <*> expression lastExpr

    Src.Case e branches ->
        Src.Case
          <$> expression e
          <*> traverse (\(p,b) -> (,) <$> validatePattern p <*> expression b) branches

    Src.Ctor name args ->
        Src.Ctor name <$> traverse expression args

    Src.Literal lit ->
        return (Src.Literal lit)

    Src.List expressions ->
        Src.List
          <$> traverse expression expressions

    Src.App funcExpr argExpr ->
        Src.App
          <$> expression funcExpr
          <*> expression argExpr

    Src.If branches finally ->
        Src.If
          <$> traverse both branches
          <*> expression finally

    Src.Access record field ->
        Src.Access
          <$> expression record
          <*> return field

    Src.Update record fields ->
        do  checkDuplicateFields ann fields
            Src.Update <$> expression record <*> traverse second fields

    Src.Record fields ->
        do  checkDuplicateFields ann fields
            Src.Record <$> traverse second fields

    Src.Let defs body ->
        Src.Let
          <$> definitions defs
          <*> expression body

    Src.GLShader uid src gltipe ->
        return (Src.GLShader uid src gltipe)


second :: (a, Src.RawExpr) -> Result w (a, Src.ValidExpr)
second (value, expr) =
    (,) value <$> expression expr


both :: (Src.RawExpr, Src.RawExpr) -> Result w (Src.ValidExpr, Src.ValidExpr)
both (expr1, expr2) =
    (,) <$> expression expr1 <*> expression expr2


checkDuplicateFields :: R.Region -> [(A.Located Text, a)] -> Result w ()
checkDuplicateFields recordRegion fields =
  let
    checkDups seenFields ( A.A region field, _ ) =
      if Set.member field seenFields then
        Result.throw recordRegion (Error.DuplicateFieldName region field)
      else
        Result.ok (Set.insert field seenFields)
  in
    foldM_ checkDups Set.empty fields



-- VALIDATE PATTERNS


validatePattern :: Pattern.Raw -> Result w Pattern.Raw
validatePattern pattern =
  do  detectDuplicates Error.BadPattern (Pattern.boundVars pattern)
      return pattern



-- DETECT DUPLICATES


checkDuplicates :: ValidStuff -> Result w ()
checkDuplicates (ValidStuff ports (D.Decls defs unions aliases _)) =
  let
    -- SIMPLE NAMES

    defValues =
      concatMap (Pattern.boundVars . Src.getPattern . A.drop) defs

    portValues =
      map fromPort ports

    fromPort (A.A (region, _) (Effects.PortRaw name _)) =
      A.A region name

    -- TYPE NAMES

    (types, typeValues) =
      unzip (map fromUnion unions ++ map fromAlias aliases)

    fromUnion (A.A (region, _) (D.Type name _ ctors)) =
      ( A.A region name
      , map (A.A region . fst) ctors
      )

    fromAlias (A.A (region, _) (D.Type name _ (A.A _ tipe))) =
      (,) (A.A region name) $
        case tipe of
          Type.RRecord _ _ ->
            [A.A region name]

          _ ->
            []
  in
    F.sequenceA_
      [ detectDuplicates Error.DuplicateValueDeclaration (portValues ++ defValues)
      , detectDuplicates Error.DuplicateValueDeclaration (concat typeValues)
      , detectDuplicates Error.DuplicateTypeDeclaration types
      ]


detectDuplicates :: (Text -> Error.Error) -> [A.Located Text] -> Result w ()
detectDuplicates tag names =
  let
    add (A.A region name) dict =
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



-- UNBOUND TYPE VARIABLES


checkTypeVarsInUnion :: A.Commented (D.Union Type.Raw) -> Result w ()
checkTypeVarsInUnion (A.A (region,_) (D.Type name boundVars ctors)) =
  do  allFreeVars <- concat <$> traverse freeVars (concatMap snd ctors)
      case diff boundVars allFreeVars of
        (_, []) ->
            return ()

        (_, unbound) ->
            Result.throw region
              (Error.UnboundTypeVarsInUnion name boundVars unbound)


checkTypeVarsInAlias :: A.Commented (D.Alias Type.Raw) -> Result w ()
checkTypeVarsInAlias (A.A (region,_) (D.Type name boundVars tipe)) =
  do  allFreeVars <- freeVars tipe
      case diff boundVars allFreeVars of
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


diff :: [Text] -> [A.Located Text] -> ([Text], [Text])
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


freeVars :: Type.Raw -> Result w [A.Located Text]
freeVars (A.A region tipe) =
  case tipe of
    Type.RLambda t1 t2 ->
      (++) <$> freeVars t1 <*> freeVars t2

    Type.RVar x ->
      Result.ok [A.A region x]

    Type.RType _ args ->
      concat <$> traverse freeVars args

    Type.RRecord fields ext ->
      do  checkDuplicateFields region fields
          let types = Maybe.maybeToList ext ++ map snd fields
          concat <$> traverse freeVars types


checkDuplicateFieldsInType :: Type.Raw -> Result w ()
checkDuplicateFieldsInType (A.A region tipe) =
  case tipe of
    Type.RLambda t1 t2 ->
      checkDuplicateFieldsInType t1
      <* checkDuplicateFieldsInType t2

    Type.RVar _ ->
      Result.ok ()

    Type.RType _ args ->
      F.traverse_ checkDuplicateFieldsInType args

    Type.RRecord fields ext ->
      do  checkDuplicateFields region fields
          let types = Maybe.maybeToList ext ++ map snd fields
          F.traverse_ checkDuplicateFieldsInType types