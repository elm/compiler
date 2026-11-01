{-# LANGUAGE QuasiQuotes, Rank2Types #-}
module Canonicalize.Effects
  ( Result(..)
  , canonicalize
  , checkPayload
  )
  where

import qualified Data.List.Utils as List
import qualified Data.Map as Map
import qualified Data.OneOrMore as OOM

import qualified Array as A

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Mode as Mode
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Type as Type
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E



-- CANONICALIZE


newtype Result a =
  Result (forall r. (a -> r) -> (OOM.OneOrMore E.Error -> r) -> r)


canonicalize :: Env.Env -> Mode.Mode -> [Src.Value] -> Map.Map T.Name I.Union -> [Src.Port] -> Src.Style -> Result Can.Style
canonicalize env mode values unions ports style =
  case style of
    Src.Normal region ->
      case ports of
        [] ->
          pure Can.Normal

        Src.Port name _ : _ ->
          case mode of
            Mode.Package _   -> throw $ E.NoPortsInPackage name
            Mode.Application -> throw $ E.UnexpectedPort region

    Src.Ports region ->
      case mode of
        Mode.Package _ ->
          throw $ E.NoPortModulesInPackage region

        Mode.Application ->
          case ports of
            []  -> throw $ E.NoPorts region
            _:_ -> Can.Ports . Map.fromList <$> traverse (canPort env) ports

    Src.Effect region manager ->
      if Mode.isKernel mode then
        case ports of
          []  -> go region manager
          _:_ -> throw $ E.UnexpectedPort region
      else
        throw $ E.NoEffectsOutsideKernel region
  where
    go region manager =
      let dict = Map.fromList (map toNameRegion values) in
      Can.Effect
        <$> verifyManager region dict [N.ascii|init|]
        <*> verifyManager region dict [N.ascii|onEffects|]
        <*> verifyManager region dict [N.ascii|onSelfMsg|]
        <*>
          case manager of
            Src.Cmd cmd ->
              Can.Cmd
                <$> verifyEType cmd unions
                <*  verifyManager region dict [N.ascii|cmdMap|]

            Src.Sub sub ->
              Can.Sub
                <$> verifyEType sub unions
                <*  verifyManager region dict [N.ascii|subMap|]

            Src.Fx cmd sub ->
              Can.Fx
                <$> verifyEType cmd unions
                <*> verifyEType sub unions
                <*  verifyManager region dict [N.ascii|cmdMap|]
                <*  verifyManager region dict [N.ascii|subMap|]



-- CANONICALIZE PORT


canPort :: Env.Env -> Src.Port -> Result (N.Name, Can.Port)
canPort env (Src.Port (A.At region portName) srcType) =
    Type.canonicalize env srcType go throws
  where
    go (Can.Forall freeVars canType) =
      let
        tipe          = Can.toIType canType
        (args,result) = delambda tipe
      in
      case result of
        I.TLambda _ _ _    -> err E.NotCmdOrSub
        I.TVar _           -> err E.NotCmdOrSub
        I.TAlias _ _ _ _ _ -> err E.NotCmdOrSub
        I.TUnit            -> err E.NotCmdOrSub
        I.TPair   _ _      -> err E.NotCmdOrSub
        I.TTriple _ _ _    -> err E.NotCmdOrSub
        I.TEmpty           -> err E.NotCmdOrSub
        I.TRecord _        -> err E.NotCmdOrSub
        I.TExtRec _ _      -> err E.NotCmdOrSub
        I.TType  h n _ xs
          | h == ModuleName.platform_cmd && n == T.cmd -> goCmd freeVars args (A.get 0 xs) tipe
          | h == ModuleName.platform_sub && n == T.sub -> goSub freeVars args (A.get 0 xs) tipe
          | otherwise                                  -> err E.NotCmdOrSub

    err x =
      throw (E.PortTypeInvalid region portName x)

    goCmd freeVars args msg tipe =
      case args of
        [] ->
          err E.CmdNoArg

        [outgoingType] ->
          case msg of
            I.TVar _ ->
              case checkPayload freeVars outgoingType of
                Right () -> pure (portName, Can.Outgoing freeVars outgoingType tipe)
                Left x   -> throw (E.PortPayloadInvalid region portName x)

            _ ->
              err E.CmdBadMsg

        _ ->
          err (E.CmdExtraArgs (length args))

    goSub freeVars args msg tipe =
      case args of
        [I.TLambda incomingType (I.TVar msg1) []] ->
          case msg of
            I.TVar msg2 | msg1 == msg2 ->
              case checkPayload freeVars incomingType of
                Right () -> pure (portName, Can.Incoming freeVars incomingType tipe)
                Left x   -> throw (E.PortPayloadInvalid region portName x)

            _ ->
              err E.SubBad

        _ ->
          err E.SubBad


delambda :: I.Type -> ([I.Type], I.Type)
delambda tipe =
  case t of
    I.TLambda x y zs   -> (List.init1 x (y:zs), List.last1 y zs)
    I.TVar _           -> ([], t)
    I.TType  _ _ _ _   -> ([], t)
    I.TAlias _ _ _ _ _ -> ([], t)
    I.TUnit            -> ([], t)
    I.TPair   _ _      -> ([], t)
    I.TTriple _ _ _    -> ([], t)
    I.TEmpty           -> ([], t)
    I.TRecord _        -> ([], t)
    I.TExtRec _ _      -> ([], t)
  where
    t = I.deepDealias tipe



-- VERIFY MANAGER


verifyEType :: A.Located T.Name -> Map.Map T.Name I.Union -> Result (Can.EType k)
verifyEType (A.At r n) unions =
  case Map.lookup n unions of
    Just (I.Union m _ _) -> pure $ Can.EType n m
    Nothing              -> throw $ E.EffectNotFound r n


toNameRegion :: Src.Value -> (N.Name, A.Region)
toNameRegion (Src.Value (A.At region name) _ _ _) =
  (name, region)


verifyManager :: A.Region -> Map.Map N.Name A.Region -> N.Name -> Result A.Region
verifyManager tagRegion values name =
  case Map.lookup name values of
    Just r  -> pure r
    Nothing -> throw (E.EffectFunctionNotFound tagRegion name)



-- CHECK PAYLOAD TYPES


checkPayload :: A.Array T.Var -> I.Type -> Either E.InvalidPayload ()
checkPayload freeVars =
    go
  where
    go tipe =
      case tipe of
        I.TLambda _ _ _     -> Left $ E.Function
        I.TVar x            -> Left $ E.TypeVariable (T.get x freeVars)
        I.TType  h n _ xs   -> goType h n xs
        I.TAlias _ _ _ xs t -> go (I.dealias xs t)
        I.TEmpty            -> Right ()
        I.TUnit             -> Right ()
        I.TPair   a b       -> go a *> go b
        I.TTriple a b c     -> go a *> go b *> go c
        I.TRecord fs        -> F.traverse_ go fs
        I.TExtRec _ _       -> Left $ E.ExtendedRecord

    goType h n xs =
      case A.toList xs of
        []
          | isJson         h n -> Right ()
          | isString       h n -> Right ()
          | isIntFloatBool h n -> Right ()

        [arg]
          | isList  h n -> go arg
          | isMaybe h n -> go arg
          | isArray h n -> go arg

        _ ->
          Left $ E.UnsupportedType n


isIntFloatBool :: ModuleName.Canonical -> T.Name -> Bool
isIntFloatBool h n =
  h == ModuleName.basics && (n == T.int || n == T.float || n == T.bool)


isString :: ModuleName.Canonical -> T.Name -> Bool
isJson   :: ModuleName.Canonical -> T.Name -> Bool
isList   :: ModuleName.Canonical -> T.Name -> Bool
isMaybe  :: ModuleName.Canonical -> T.Name -> Bool
isArray  :: ModuleName.Canonical -> T.Name -> Bool

isString h n = h == ModuleName.string     && n == T.string
isJson   h n = h == ModuleName.jsonEncode && n == T.value
isList   h n = h == ModuleName.list       && n == T.list
isMaybe  h n = h == ModuleName.maybe      && n == T.maybe
isArray  h n = h == ModuleName.array      && n == T.array



-- RESULT HELPERS


throw :: E.Error -> Result a
throw x =
  Result $ \_ err -> err (OOM.one x)


throws :: OOM.OneOrMore E.Problem -> Result a
throws xs =
  Result $ \_ err -> err (OOM.map E.Problem xs)


instance Functor Result where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \ok err -> k (ok . func) err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \ok _ -> ok a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \ok err ->
      kF
        (\func -> kA (ok . func) err)
        (\xs -> kA (\_ -> err xs) (\ys -> err (OOM.more xs ys)))
