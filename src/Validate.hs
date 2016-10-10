{-# OPTIONS_GHC -Wall #-}
module Validate (Result, module') where

import Control.Monad (foldM_, when)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import AST.Expression.General as Expr
import qualified AST.Effects as Effects
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



type Result warning a =
  Result.Result () warning Error.Error a



-- MODULES


module' :: Module.Source -> Result wrn Module.Valid
module' (Module.Module name path info) =
  let
    (ModuleName.Canonical pkgName _) =
      name

    (Module.Source tag settings docs exports imports decls) =
      info
  in
    do  (ValidStuff ports structure) <- validateDecls decls

        validEffects <- validateEffects tag settings ports (D._defs structure)

        return $ Module.Module name path $
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
  -> [A.Commented Valid.Def]
  -> Result wrn Effects.Raw
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


noSettings :: Error.Error -> Module.SourceSettings -> Result wrn ()
noSettings errorMsg (A.A region settings) =
  case settings of
    [] ->
      Result.ok ()

    _ : _ ->
      Result.throw region errorMsg


noPorts :: [A.Commented Effects.PortRaw] -> Result wrn ()
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
  -> Map.Map String [(R.Region, A.Located String)]
  -> Result wrn Effects.ManagerType
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
  :: String
  -> Map.Map String [(R.Region, A.Located String)]
  -> Result wrn (Maybe (A.Located String))
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
  -> Effects.ManagerType
  -> [A.Commented Valid.Def]
  -> Result wrn (R.Region, R.Region, R.Region)
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


getSimpleDefRegion :: A.Commented Valid.Def -> Maybe (String, R.Region)
getSimpleDefRegion decl =
  case decl of
    A.A _ (Valid.Def region (A.A _ (Pattern.Var name)) _ _) ->
      Just (name, region)

    _ ->
      Nothing


requireMaps
  :: R.Region
  -> Map.Map String R.Region
  -> Effects.ManagerType
  -> Result wrn ()
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


requireRegion :: R.Region -> Map.Map String R.Region -> String -> Result wrn R.Region
requireRegion tagRegion regionDict name =
  case Map.lookup name regionDict of
    Just region ->
      return region

    Nothing ->
      Result.throw tagRegion (Error.MissingManagerOnEffectModule name)



-- COLLAPSE COMMENTS


collapseComments :: [D.CommentOr (A.Located a)] -> Result wrn [A.Commented a]
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


validateDecls :: [D.Source] -> Result wrn ValidStuff
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


validateRawDecls :: [A.Commented D.Raw] -> Result wrn ValidStuff
validateRawDecls commentedDecls =
  vrdHelp commentedDecls [] (D.Decls [] [] [] [])


vrdHelp
  :: [A.Commented D.Raw]
  -> [A.Commented Effects.PortRaw]
  -> D.Valid
  -> Result wrn ValidStuff
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
          vrdHelp rest (A.A ann (Effects.PortRaw name tipe) : ports) structure


vrdDefHelp
  :: [A.Commented D.Raw]
  -> A.Commented Source.Def'
  -> [A.Commented Effects.PortRaw]
  -> D.Valid
  -> Result wrn ValidStuff
vrdDefHelp remainingDecls (A.A ann def) ports structure =
  let
    addDef validDef (ValidStuff finalPorts struct) =
      ValidStuff finalPorts (D.addDef (A.A ann validDef) struct)
  in
    case def of
      Source.Definition pat expr ->
        addDef
          <$> validateDef (fst ann) pat expr Nothing
          <*> vrdHelp remainingDecls ports structure

      Source.Annotation name tipe ->
        case remainingDecls of
          A.A _ (D.Def (A.A defRegion (Source.Definition pat expr))) : rest
           | Pattern.isVar name pat ->
              addDef
                <$> validateDef (R.merge (fst ann) defRegion) pat expr (Just tipe)
                <*> vrdHelp rest ports structure

          _ ->
            vrdHelp remainingDecls ports structure
              <* Result.throw (fst ann) (Error.TypeWithoutDefinition name)



-- VALIDATE DEFINITIONS


definitions :: [Source.Def] -> Result wrn [Valid.Def]
definitions sourceDefs =
  do  validDefs <- definitionsHelp sourceDefs

      validDefs
        |> map Valid.getPattern
        |> concatMap Pattern.boundVars
        |> detectDuplicates Error.DuplicateDefinition

      return validDefs


definitionsHelp :: [Source.Def] -> Result wrn [Valid.Def]
definitionsHelp sourceDefs =
  case sourceDefs of
    [] ->
      return []

    A.A defRegion (Source.Definition pat expr) : rest ->
      (:)
        <$> validateDef defRegion pat expr Nothing
        <*> definitionsHelp rest

    A.A annRegion (Source.Annotation name tipe) : rest ->
      case rest of
        A.A defRegion (Source.Definition pat expr) : rest'
          | Pattern.isVar name pat ->
              (:)
                <$> validateDef (R.merge annRegion defRegion) pat expr (Just tipe)
                <*> definitionsHelp rest'

        _ ->
          Result.throw annRegion (Error.TypeWithoutDefinition name)


validateDef
  :: R.Region
  -> Pattern.Raw
  -> Source.Expr
  -> Maybe Type.Raw
  -> Result wrn Valid.Def
validateDef region pat expr maybeType =
  do  validExpr <- expression expr
      validateDefPattern pat validExpr
      return $ Valid.Def region pat validExpr maybeType


validateDefPattern :: Pattern.Raw -> Valid.Expr -> Result wrn ()
validateDefPattern pattern body =
  case fst (Expr.collectLambdas body) of
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


checkArguments :: String -> [Pattern.Raw] -> Result wrn ()
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


expression :: Source.Expr -> Result wrn Valid.Expr
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
          do  foldM_ checkDups Set.empty fields
              Record <$> T.traverse second fields

    Let defs body ->
        Let
          <$> definitions defs
          <*> expression body

    Cmd moduleName ->
        return (Cmd moduleName)

    Sub moduleName ->
        return (Sub moduleName)

    OutgoingPort name tipe ->
        return (OutgoingPort name tipe)

    IncomingPort name tipe ->
        return (IncomingPort name tipe)

    Program _ _ ->
        error "DANGER - Program AST nodes should not be in validation phase."

    SaveEnv moduleName effects ->
        return (SaveEnv moduleName effects)

    GLShader uid src gltipe ->
        return (GLShader uid src gltipe)


second :: (a, Source.Expr) -> Result wrn (a, Valid.Expr)
second (value, expr) =
    (,) value <$> expression expr


both :: (Source.Expr, Source.Expr) -> Result wrn (Valid.Expr, Valid.Expr)
both (expr1, expr2) =
    (,) <$> expression expr1 <*> expression expr2



-- VALIDATE PATTERNS


validatePattern :: Pattern.Raw -> Result wrn Pattern.Raw
validatePattern pattern =
  do  detectDuplicates Error.BadPattern (Pattern.boundVars pattern)
      return pattern



-- DETECT DUPLICATES


checkDuplicates :: ValidStuff -> Result wrn ()
checkDuplicates (ValidStuff ports (D.Decls defs unions aliases _)) =
  let
    -- SIMPLE NAMES

    defValues =
      concatMap (Pattern.boundVars . Valid.getPattern . A.drop) defs

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


detectDuplicates :: (String -> Error.Error) -> [A.Located String] -> Result wrn ()
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


checkTypeVarsInUnion :: A.Commented (D.Union Type.Raw) -> Result wrn ()
checkTypeVarsInUnion (A.A (region,_) (D.Type name boundVars ctors)) =
  case diff boundVars (concatMap freeVars (concatMap snd ctors)) of
    (_, []) ->
        return ()

    (_, unbound) ->
        Result.throw region
          (Error.UnboundTypeVarsInUnion name boundVars unbound)


checkTypeVarsInAlias :: A.Commented (D.Alias Type.Raw) -> Result wrn ()
checkTypeVarsInAlias (A.A (region,_) (D.Type name boundVars tipe)) =
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
