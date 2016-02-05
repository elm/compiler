{-# OPTIONS_GHC -Wall #-}
module Validate (declarations) where

import qualified Control.Arrow as Arrow
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
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



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



-- PARTITION AND VALIDATE DEFINES


type Defines =
  Map.Map D.EffectType [A.Located [D.CommentOr (A.Located Source.Effect)]]


partitionEffects :: [Commented D.SourceOrDefine] -> ([Commented D.Source'], Defines)
partitionEffects commentedDecls =
  case commentedDecls of
    [] ->
      ([], Map.empty)

    A.A ann (D.Source decl) : rest ->
      Arrow.first
        ((:) (A.A ann decl))
        (partitionEffects rest)

    A.A (region, _) (D.Define effectType commentOrEffects) : rest ->
      Arrow.second
        (Map.insertWith (++) effectType [A.A region commentOrEffects])
        (partitionEffects rest)


validateDefines
  :: Module.Tag
  -> Defines
  -> Result.Result wrn Error.Error Valid.Effects
validateDefines tag defines =
  case tag of
    Module.None ->
      do  disallowLeftoverDefines Error.BadDefineInNormalModule defines
          return Valid.None

    Module.Effect region ->
      let
        (maybeCmds, smallerDefines) =
          extractKey D.Cmd defines

        (maybeSubs, leftovers) =
          extractKey D.Sub smallerDefines
      in
        do  disallowLeftoverDefines Error.BadDefineInEffectModule leftovers
            case (maybeCmds, maybeSubs) of
              (Nothing, Nothing) ->
                Result.throw region Error.NoDefineInEffectModule

              _ ->

                Valid.Effects
                  <$> validateDefineDefs D.Cmd maybeCmds validateEffects
                  <*> validateDefineDefs D.Sub maybeSubs validateEffects

    Module.Foreign region ->
      let
        (maybeCmds, smallerDefines) =
          extractKey D.ForeignCmd defines

        (maybeSubs, leftovers) =
          extractKey D.ForeignSub smallerDefines
      in
        do  disallowLeftoverDefines Error.BadDefineInForeignModule leftovers
            case (maybeCmds, maybeSubs) of
              (Nothing, Nothing) ->
                Result.throw region Error.NoDefineInForeignModule

              (_, _) ->
                Valid.ForeignEffects
                  <$> validateDefineDefs D.ForeignCmd maybeCmds (T.traverse validateForeignCmd)
                  <*> validateDefineDefs D.ForeignSub maybeSubs (T.traverse validateForeignSub)


extractKey :: (Ord k) => k -> Map.Map k v -> (Maybe v, Map.Map k v)
extractKey key dict =
  ( Map.lookup key dict
  , Map.delete key dict
  )


validateDefineDefs
  :: D.EffectType
  -> Maybe [A.Located [D.CommentOr (A.Located Source.Effect)]]
  -> ([Commented Source.Effect] -> Result.Result wrn Error.Error a)
  -> Result.Result wrn Error.Error a
validateDefineDefs effectType maybeAllRawDefines validate =
  case maybeAllRawDefines of
    Nothing ->
      validate []

    Just [A.A _ sourceDefs] ->
      validate =<< collapseComments sourceDefs

    Just allRawDefines ->
      let
        (A.A region _) =
          last allRawDefines

        errorMsg =
          Error.DuplicateDefines effectType (length allRawDefines)
      in
        Result.throw region errorMsg


disallowLeftoverDefines
  :: (D.EffectType -> Error.Error)
  -> Defines
  -> Result.Result wrn Error.Error ()
disallowLeftoverDefines errorTagger leftoverDefines =
  case Map.toList leftoverDefines of
    [] ->
      Result.ok ()

    pairs ->
      let
        toErrors (key, allDefines) =
          map (A.map (\_ -> errorTagger key)) allDefines
      in
        Result.throwMany (concatMap toErrors pairs)



-- VALIDATE STRUCTURED SOURCE


declarations
  :: Module.Tag
  -> [D.Source]
  -> Result.Result wrn Error.Error ([D.Valid], Valid.Effects)
declarations tag sourceDecls =
  do  (decls, defines) <-
        partitionEffects <$> collapseComments sourceDecls

      validDecls <-
        validateDecls decls

      validEffects <-
        validateDefines tag defines

      F.traverse_ id
        (declDuplicates validDecls validEffects : map findBadTypeVars validDecls)

      return (validDecls, validEffects)



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



-- VALIDATE EFFECTS


validateEffects
  :: [Commented Source.Effect]
  -> Result.Result wrn Error.Error [A.Commented Valid.Def]
validateEffects effects =
  case effects of
    [] ->
      Result.ok []

    A.A (region, maybeComment) effect : rest ->
      validateEffectsHelp (region, fmap A.drop maybeComment) effect rest


validateEffectsHelp
  :: (R.Region, Maybe String)
  -> Source.Effect
  -> [Commented Source.Effect]
  -> Result.Result wrn Error.Error [A.Commented Valid.Def]
validateEffectsHelp ann effect remainingEffects =
  case effect of
    Source.With _ _ ->
      Result.throw (fst ann) (error "TODO")

    Source.Def pat expr ->
      (:)
        <$> validateDef Valid.Definition ann pat expr Nothing
        <*> validateEffects remainingEffects

    Source.Type name tipe ->
      case remainingEffects of
        A.A (_, Just (A.A commentRegion _)) _ : _ ->
          Result.throw commentRegion (Error.CommentAfterAnnotation name)

        A.A _ (Source.Def pat expr) : rest
         | Pattern.isVar name pat ->
            (:)
              <$> validateDef Valid.Definition ann pat expr (Just tipe)
              <*> validateEffects rest

        _ ->
          Result.throw (fst ann) (Error.TypeWithoutDefinition name)


validateForeignCmd
  :: Commented Source.Effect
  -> Result.Result wrn Error.Error (A.Commented Valid.ForeignCmd)
validateForeignCmd (A.A (region, maybeComment) effect) =
  case effect of
    Source.With _ _ ->
      Result.throw region (error "TODO")

    Source.Def _ _ ->
      Result.throw region (error "TODO")

    Source.Type name tipe ->
      Result.ok $
        A.A (region, fmap A.drop maybeComment) (Valid.Cmd name tipe)


validateForeignSub
  :: Commented Source.Effect
  -> Result.Result wrn Error.Error (A.Commented Valid.ForeignSub)
validateForeignSub (A.A (region, maybeComment) effect) =
  case effect of
    Source.With name expr ->
      do  validExpr <- expression expr
          Result.ok $
            A.A (region, fmap A.drop maybeComment) (Valid.SubWith name validExpr)

    Source.Def _ _ ->
      Result.throw region (error "TODO")

    Source.Type name tipe ->
      Result.ok $
        A.A (region, fmap A.drop maybeComment) (Valid.Sub name tipe)



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


declDuplicates :: [D.Valid] -> Valid.Effects -> Result.Result wrn Error.Error ()
declDuplicates decls effects =
  let
    (valueLists, typeLists) =
      unzip (map extractValues decls)

    effectValues =
      extractEffectValues effects
  in
    (\_ _ -> ())
      <$> detectDuplicates Error.DuplicateValueDeclaration (effectValues ++ concat valueLists)
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


extractEffectValues :: Valid.Effects -> [A.Located String]
extractEffectValues effects =
  case effects of
    Valid.None ->
      []

    Valid.Effects cmds subs ->
      let
        extractNames (Valid.Definition pat _ _) =
          Pattern.boundVars pat
      in
        concatMap (extractNames . A.drop) (cmds ++ subs)

    Valid.ForeignEffects cmds subs ->
      let
        extractCmdNames (A.A (region, _) (Valid.Cmd name _)) =
          A.A region name

        extractSubNames (A.A (region, _) sub) =
          A.A region $
            case sub of
              Valid.Sub name _ ->
                name

              Valid.SubWith name _ ->
                name
      in
        map extractCmdNames cmds ++ map extractSubNames subs



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

