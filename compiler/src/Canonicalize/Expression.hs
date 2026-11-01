{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, QuasiQuotes, Rank2Types,
TemplateHaskell, UnboxedTuples
#-}
module Canonicalize.Expression
  ( Result(..)
  , Usage(..)
  , canonicalize
  , addLocals
  )
  where


import qualified Data.Dups as Dups
import qualified Data.List as List
import qualified Data.List.Utils as List
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.OneOrMore as OOM
import GHC.Int (Int(..))
import GHC.Prim
import GHC.ST (ST(ST), runST)

import qualified Crash
import qualified Graph
import qualified String as S

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeName as T
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Pattern as Pattern
import qualified Canonicalize.Type as Type
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E
import qualified Reporting.Warning as W



-- RESULT
--
-- This tracks uses of local variables and top level definitions. The usage
-- info is needed when enforcing the recursion rules for browser code.


newtype Result a =
  Result
  (
    forall r.
      Locals
      -> Tops
      -> [W.Warning]
      -> (Locals -> Tops -> [W.Warning] -> a -> r)
      -> ([W.Warning] -> OOM.OneOrMore E.Problem -> r)
      -> r
  )


type Locals = Map.Map N.Name ()

type Tops = Map.Map N.Name Usage

data Usage = Direct | Delayed



-- CANONICALIZE


canonicalize :: Env.Env -> Usage -> Src.Expr -> Result Can.Expr
canonicalize env usage =
    go
  where
    go (A.At r expr) =
      A.At r <$>
      case expr of
        Src.Str s         -> pure $ Can.Str s
        Src.Chr c         -> pure $ Can.Chr c
        Src.Int i         -> pure $ Can.Int (fromIntegral i) -- TODO error on overflow
        Src.Float f       -> pure $ Can.Float f
        Src.Unit          -> pure $ Can.Unit
        Src.Empty         -> pure $ Can.Empty
        Src.Accessor f    -> pure $ Can.Accessor f
        Src.Shader s t    -> pure $ Can.Shader s t
        Src.Negate e      -> Can.Negate <$> go e
        Src.List es       -> Can.List   <$> traverse go es
        Src.Call f x xs   -> Can.Call   <$> go f <*> go x <*> traverse go xs
        Src.If bs e       -> Can.If     <$> traverse goThen bs <*> go e
        Src.Case e b bs   -> Can.Case   <$> go e <*> goBranch b <*> traverse goBranch bs
        Src.Record f fs   -> Can.Record <$> canFields env usage (\_ e -> e) f fs
        Src.Access e f    -> Can.Access <$> go e <*> pure f
        Src.Var t n       -> canVar    env usage r t n
        Src.VarQual t p n -> canQual   env       r t p n
        Src.Op op         -> canOp env r op
        Src.Binops ops e  -> canBinops env usage r ops e
        Src.Lambda p ps e -> canLambda env         p ps e
        Src.Let defs e    -> canLet    env usage r defs e
        Src.Tuple a b cs  -> canTuple  env usage r a b cs
        Src.Update n f fs -> canUpdate env usage n f fs

    goThen (c,b) = (,) <$> go c <*> go b
    goBranch     = canBranch env usage



-- CANONICALIZE OPERATOR


canOp :: Env.Env -> A.Region -> Op.Name -> Result Can.Expr_
canOp env region op =
  do  (Env.Binop h n t _ _) <- findBinop env region op
      return $ Can.VarOperator op h n t


findBinop :: Env.Env -> A.Region -> Op.Name -> Result Env.Binop
findBinop (Env.Env _ _ _ _ binops _ _ _) region op =
  case Map.lookup op binops of
    Just binop -> pure binop
    Nothing    -> throw $ E.NotFoundBinop region op (Map.keysSet binops)



-- CANONICALIZE LAMBDA


canLambda :: Env.Env -> Src.Pattern -> [Src.Pattern] -> Src.Expr -> Result Can.Expr_
canLambda env p ps expr =
  bind env E.DPLambdaArgs
    (\env1 -> List.traverse1 (Pattern.canonicalize env1) p ps)
    (\env2 -> canonicalize env2 Delayed expr)
    (\_ (x,xs) body -> Can.Lambda x xs body)



-- CANONICALIZE CASE BRANCH


canBranch :: Env.Env -> Usage -> Src.Branch -> Result Can.Branch
canBranch env usage (Src.Branch pattern expr) =
  bind env E.DPCaseBranch
    (\env1 -> Pattern.canonicalize env1 pattern)
    (\env2 -> canonicalize env2 usage expr)
    (\_ p e -> Can.Branch p e)



-- CANONICALIZE TUPLE


canTuple :: Env.Env -> Usage -> A.Region -> Src.Expr -> Src.Expr -> [Src.Expr] -> Result Can.Expr_
canTuple env usage region a b cs =
  case cs of
    []  -> Can.Pair   <$> go a <*> go b
    [c] -> Can.Triple <$> go a <*> go b <*> go c
    _   -> throw $ E.TupleLargerThanThree region
  where
    go = canonicalize env usage



-- CANONICALIZE RECORD


canFields :: Env.Env -> Usage -> (A.Region -> Can.Expr -> a) -> (A.Located N.Name, Src.Expr) -> [(A.Located N.Name, Src.Expr)] -> Result (F.Fields a)
canFields env usage mk field fields =
  case Dups.detect1 (\_ _ e -> e) E.DuplicateField dict of
    Dups.Detector k ->
      k (F.fromOrdered can) throws
  where
    dict                = List.foldl' add (one field) fields
    one   (A.At r n, e) = Dups.one1    n r (F.Ordered 0 (r,e))
    add d (A.At r n, e) = Dups.insert1 n r (F.Ordered (fromIntegral (Dups.size1 d)) (r,e)) d
    can   (     r  , e) = mk r <$> canonicalize env usage e


canUpdate :: Env.Env -> Usage -> A.Located N.Name -> (A.Located N.Name, Src.Expr) -> [(A.Located N.Name, Src.Expr)] -> Result Can.Expr_
canUpdate env usage (A.At region name) field fields =
  Can.Update name
    <$> (A.At region <$> canVarLow env usage region name)
    <*> canFields env usage Can.Update_ field fields



-- CANONICALIZE VARS


canVar :: Env.Env -> Usage -> A.Region -> Src.VarType -> N.Name -> Result Can.Expr_
canVar env usage r varType name =
  case varType of
    Src.LowVar -> canVarLow env usage r name
    Src.CapVar -> Env.withCtor env r name (toUnionCtor name) (toAliasCtor name) throw


canQual :: Env.Env -> A.Region -> Src.VarType -> Module.Prefix -> N.Name -> Result Can.Expr_
canQual env r varType prefix name =
  case varType of
    Src.LowVar -> canQualLow env r prefix name
    Src.CapVar -> Env.withCtorQual env r prefix name (toUnionCtor name) (toAliasCtor name) throw


toUnionCtor :: N.Name -> ModuleName.Canonical -> Env.UnionCtor -> Result Can.Expr_
toUnionCtor name home ctor =
  case ctor of
    Env.Variant anno _ tag len _ -> pure $ Can.CVariant home name anno tag len
    Env.Enum    anno i tag       -> pure $ goEnum anno i tag
    Env.Vector  anno _ len _ _ _ -> pure $ Can.CVector  home name anno len
    Env.Box     anno _ _         -> pure $ Can.CBox     home name anno
    Env.Void    anno _           -> pure $ Can.CVoid    home name anno
  where
    goEnum anno (Can.PType h n _ _ _) tag =
      if n == T.bool && h == ModuleName.basics
      then Can.Bool (tag == Tag.zero)
      else Can.CEnum home name anno tag


toAliasCtor :: N.Name -> ModuleName.Canonical -> Env.AliasCtor -> Result Can.Expr_
toAliasCtor name home (Env.Record anno f fs) =
  pure $ Can.CRecord home name anno f fs



-- CANONICALIZE LOW VARIABLES


canVarLow :: Env.Env -> Usage -> A.Region -> N.Name -> Result Can.Expr_
canVarLow (Env.Env home vs _ _ _ qvs _ _) usage region name =
  case Map.lookup name vs of
    Just var ->
      case var of
        Env.Local _      -> addLocal name
        Env.TopLevel _   -> addTop home name usage
        Env.Foreign  h a -> pure $ canForeign home h name a
        Env.Foreigns amb -> throw $ E.AmbiguousVar region Nothing name amb

    Nothing ->
      throw $ E.NotFoundVar region Nothing name (Env.toPossibleNames vs qvs)


canQualLow :: Env.Env -> A.Region -> Module.Prefix -> N.Name -> Result Can.Expr_
canQualLow (Env.Env home vs _ _ _ qvs _ _) region prefix name =
  case Map.lookup prefix qvs of
    Just qualified ->
      case Map.lookup name qualified of
        Just info ->
          case info of
            Env.ForeignValue  h a -> pure $ canForeign home h name a
            Env.ForeignValues amb -> throw $ E.AmbiguousVar region (Just prefix) name amb

        Nothing ->
          throwNotFoundVar

    Nothing ->
      let prefixName = Module.fromString (Module.prefixToString prefix) in
      if Module.isKernel prefixName && Pkg.isKernel (ModuleName._package home)
      then pure $ Can.VarKernel (Module.getKernel prefixName) name
      else throwNotFoundVar
  where
    throwNotFoundVar =
      throw $ E.NotFoundVar region (Just prefix) name (Env.toPossibleNames vs qvs)


canForeign :: ModuleName.Canonical -> ModuleName.Canonical -> N.Name -> I.Annotation -> Can.Expr_
canForeign localHome home name annotation =
  if home == ModuleName.debug
  then Can.VarDebug localHome name annotation
  else Can.VarForeign home name annotation


addLocal :: N.Name -> Result Can.Expr_
addLocal name =
  Result $ \locals tops warns ok _ ->
    ok (Map.insert name () locals) tops warns (Can.VarLocal name)


addTop :: ModuleName.Canonical -> N.Name -> Usage -> Result Can.Expr_
addTop h n usage =
  Result $ \locals tops warns ok _ ->
    ok locals (Map.insertWith merge n usage tops) warns (Can.VarTopLevel h n)



-- CANONICALIZE BINOPS


canBinops :: Env.Env -> Usage -> A.Region -> [(Src.Expr, A.Located Op.Name)] -> Src.Expr -> Result Can.Expr_
canBinops env usage overallRegion ops final =
  A.toValue <$>
  (
    runBinopStepper overallRegion =<<
      (More <$> traverse go ops <*> canonicalize env usage final)
  )
  where
    go (expr, A.At region op) =
      (\e b -> ExprOp e op b)
        <$> canonicalize env usage expr
        <*> findBinop env region op


data Step
  = Done Can.Expr
  | More [ExprOp] Can.Expr
  | Error Op.Name Op.Name


data ExprOp =
  ExprOp Can.Expr Op.Name Env.Binop


runBinopStepper :: A.Region -> Step -> Result Can.Expr
runBinopStepper overallRegion step =
  case step of
    Done expr ->
      pure expr

    More [] expr ->
      pure expr

    More (ExprOp expr op binop : rest) final ->
      runBinopStepper overallRegion $
        toBinopStep (toBinop op binop expr) op binop rest final

    Error op1 op2 ->
      throw $ E.Binop overallRegion op1 op2


toBinopStep :: (Can.Expr -> Can.Expr) -> Op.Name -> Env.Binop -> [ExprOp] -> Can.Expr -> Step
toBinopStep makeBinop rootOp rootBinop@(Env.Binop _ _ _ rootAssoc rootPrec) middle final =
  case middle of
    [] ->
      Done (makeBinop final)

    ExprOp expr op binop@(Env.Binop _ _ _ assoc prec) : rest ->
      if prec < rootPrec then

        More (ExprOp (makeBinop expr) op binop : rest) final

      else if prec > rootPrec then

        case toBinopStep (toBinop op binop expr) op binop rest final of
          Done           newLast -> Done (makeBinop newLast)
          More newMiddle newLast -> toBinopStep makeBinop rootOp rootBinop newMiddle newLast
          Error x y              -> Error x y

      else

        case (rootAssoc, assoc) of
          (Op.Left , Op.Left ) -> toBinopStep (\right -> toBinop op binop (makeBinop expr) right) op binop rest final
          (Op.Right, Op.Right) -> toBinopStep (\right -> makeBinop (toBinop op binop expr right)) op binop rest final
          (_       , _       ) -> Error rootOp op


toBinop :: Op.Name -> Env.Binop -> Can.Expr -> Can.Expr -> Can.Expr
toBinop op (Env.Binop h n t _ _) left right =
  A.merge left right (Can.Binop op h n t left right)



--------------------------------------------------------------------------------
-- CANONICALIZE LET ------------------------------------------------------------
--------------------------------------------------------------------------------


-- CANONICALIZE LET
--
-- Let expressions need a bit of extra care including:
--
--   1. Add bound variables to environment.
--   2. Canonicalize with all bound variables available.
--   3. Disallow recursive values.


canLet :: Env.Env -> Usage -> A.Region -> [A.Located Src.Def] -> Src.Expr -> Result Can.Expr_
canLet env usage region defs body =
  withLetBindings env defs $ \newEnv ->
    do  cdefs <- traverse (canLetDef newEnv usage) defs
        cbody <- canonicalize newEnv usage body
        expr  <- sort region cdefs cbody
        return $ A.toValue expr



-- WITH LET BINDINGS


withLetBindings :: Env.Env -> [A.Located Src.Def] -> (Env.Env -> Result a) -> Result a
withLetBindings env defs callback =
  Result $ \outerLocals outerTops warnings ok err ->
    let
      ok1 bindings =
        case callback =<< addLocals bindings env of
          Result k ->
            k Map.empty Map.empty warnings ok2 err
        where
          ok2 innerLocals innerTops warnings2 a =
            let
              escapedLocals  = Map.difference innerLocals bindings
              newOuterLocals = Map.union outerLocals escapedLocals
              newOuterTops   = Map.unionWith merge outerTops innerTops
              newWarnings    = addUnused W.UnusedDef bindings escapedLocals innerLocals warnings2
            in
            ok newOuterLocals newOuterTops newWarnings a
    in
    case Dups.detect (\_ r _ -> r) (E.DuplicatePattern E.DPLetBinding) (getLetBindings defs) of
      Dups.Detector k ->
        k ok1 (err warnings)


getLetBindings :: [A.Located Src.Def] -> Dups.Dict N.Name A.Region ()
getLetBindings defs =
    List.foldl' add Dups.none defs
  where
    add dict (A.At _ def) =
      case def of
        Src.Define (A.At r n) _ _ _ -> Dups.insert n r () dict
        Src.Destruct p _            -> go dict p

    go dict (A.At region pattern) =
      case pattern of
        Src.PAnything           -> dict
        Src.PChr _              -> dict
        Src.PStr _              -> dict
        Src.PInt _              -> dict
        Src.PUnit               -> dict
        Src.PVar n              -> Dups.insert n region () dict
        Src.PAlias p (A.At r n) -> Dups.insert n r () $ go dict p
        Src.PRecord fs          -> List.foldl' goField dict fs
        Src.PTuple a b cs       -> List.foldl' go (go (go dict a) b) cs
        Src.PList ps            -> List.foldl' go dict ps
        Src.PCons p ps          -> go (go dict p) ps
        Src.PCtor _ _ ps        -> List.foldl' go dict ps
        Src.PCtorQual _ _ _ ps  -> List.foldl' go dict ps

    goField dict (A.At r n) =
      Dups.insert n r () dict



--------------------------------------------------------------------------------
-- CANONICALIZE DEFS -----------------------------------------------------------
--------------------------------------------------------------------------------


data Def
  = DValue Can.Value N.Name Locals
  | DFunc  Can.Func  N.Name Locals
  | DDestruct Can.Pattern Can.Expr (Map.Map N.Name A.Region) Locals


canLetDef :: Env.Env -> Usage -> A.Located Src.Def -> Result Def
canLetDef env usage (A.At _ def) =
  case def of
    Src.Define n xs e t -> canLetEqual env usage n xs e t
    Src.Destruct p e    -> canDestruct env usage p e



-- CANONICALIZE DEFINE


canLetEqual :: Env.Env -> Usage -> A.Located N.Name -> [Src.Pattern] -> Src.Expr -> Maybe Src.Type -> Result Def
canLetEqual env usage name@(A.At _ n) patterns expr maybeType =
  case maybeType of
    Nothing ->
      case patterns of
        [] ->
          bind env (E.DPFuncArgs n)
            (\_       -> pure ())
            (\env2    -> canonicalize env2 usage expr)
            (\vs () e -> DValue (Can.Value_ name e) n vs)

        arg:args ->
          bind env (E.DPFuncArgs n)
            (\env1        -> List.traverse1 (Pattern.canonicalize env1) arg args)
            (\env2        -> canonicalize env2 Delayed expr)
            (\vs (p,ps) e -> DFunc (Can.Func_ name p ps e) n vs)

    Just tipe ->
      Type.canonicalize env tipe (go . Can.toIAnnotation) throws

  where
    go anno@(I.Forall _ tipe) =
      case patterns of
        [] ->
          bind env (E.DPFuncArgs n)
            (\_       -> pure ())
            (\env2    -> canonicalize env2 usage expr)
            (\vs () e -> DValue (Can.Value name e tipe anno) n vs)

        arg:args ->
          bind env (E.DPFuncArgs n)
            (\env1          -> Pattern.canTypedArgs env1 n arg args tipe)
            (\env2          -> canonicalize env2 Delayed expr)
            (\vs (p,ps,t) e -> DFunc (Can.Func name p ps e t anno) n vs)



-- CANONICALIZE DESTRUCT


canDestruct :: Env.Env -> Usage -> Src.Pattern -> Src.Expr -> Result Def
canDestruct env usage srcPattern srcExpr =
  Result $ \outerLocals outerTops warnings ok err ->
    let
      ok1 dups pattern =
        case Dups.detect (\_ r _ -> r) (E.DuplicatePattern E.DPDestruct) dups of
          Dups.Detector k ->
            k ok2 (err warnings)
        where
          ok2 bindings =
            case canonicalize env usage srcExpr of
              Result k ->
                k Map.empty Map.empty warnings ok3 err
            where
              ok3 innerLocals innerTops warnings3 expr =
                let
                  !newOuterLocals = Map.union outerLocals innerLocals
                  !newOuterTops   = Map.unionWith merge outerTops innerTops
                in
                ok newOuterLocals newOuterTops warnings3 $
                  DDestruct pattern expr bindings innerLocals
    in
    case Pattern.canonicalize env srcPattern of
      Pattern.Result k ->
        k Dups.none ok1 (err warnings)



-- BIND
--
-- Free variables in function bodies are Delayed uses, whereas free variables
-- in case branches and destructure bodies are Direct uses.


bind
  :: Env.Env
  -> E.DuplicatePatternContext
  -> (Env.Env -> Pattern.Result pattern)
  -> (Env.Env -> Result expr)
  -> (Locals -> pattern -> expr -> a)
  -> Result a
bind env x toPattern toExpr toAnswer =
  Result $ \outerLocals outerTops warnings ok err ->
    let
      ok1 dups pattern =
        case Dups.detect (\_ r _ -> r) (E.DuplicatePattern x) dups of
          Dups.Detector k ->
            k ok2 (err warnings)
        where
          ok2 bindings =
            case toExpr =<< addLocals bindings env of
              Result k ->
                k Map.empty Map.empty warnings ok3 err
            where
              ok3 innerLocals innerTops warnings3 expr =
                let
                  escapedLocals  = Map.difference innerLocals bindings
                  newOuterLocals = Map.union outerLocals escapedLocals
                  newOuterTops   = Map.unionWith merge outerTops innerTops
                  newWarnings    = addUnused W.UnusedVar bindings escapedLocals innerLocals warnings3
                in
                ok newOuterLocals newOuterTops newWarnings (toAnswer escapedLocals pattern expr)
    in
    case toPattern env of
      Pattern.Result k ->
        k Dups.none ok1 (err warnings)


merge :: Usage -> Usage -> Usage
merge usage1 usage2 =
  case usage1 of
    Direct  -> usage1
    Delayed -> usage2



-- UNUSED VARS AND DEFS
--
-- The simpler approach is to compute (Map.difference bindings allLocals) and
-- then check if the result is empty. If empty, no unused variables!
--
-- Instead, this code uses Map.size to determine if that dictionary would be
-- empty in O(1) and without any allocation. That way Map.difference is only
-- computed in the relatively uncommon "bad case" when there are actually
-- warnings to report.


addUnused :: (A.Region -> N.Name -> W.Warning) -> Map.Map N.Name A.Region -> Locals -> Locals -> [W.Warning] -> [W.Warning]
addUnused warn bindings escapedLocals allLocals warnings =
    if Map.size bindings + Map.size escapedLocals == Map.size allLocals
    then warnings
    else Map.foldrWithKey cons warnings (Map.difference bindings allLocals)
  where
    cons n r ws = warn r n : ws



-- ADD LOCALS


addLocals :: Map.Map N.Name A.Region -> Env.Env -> Result Env.Env
addLocals bindings (Env.Env home values ts cs bs qvs qts qcs) =
  update <$>
    Map.mergeA (Map.mapMissing goLeft) (Map.mapMissing goRight) (Map.zipWithAMatched goBoth) bindings values
  where
    update newValues  = Env.Env home newValues ts cs bs qvs qts qcs
    goLeft _ region   = Env.Local region
    goRight _ value   = value
    goBoth n region v =
      case v of
        Env.Foreign _ _ -> pure $ Env.Local region
        Env.Foreigns _  -> pure $ Env.Local region
        Env.Local r     -> throw $ E.Shadowing n r region
        Env.TopLevel r  -> throw $ E.Shadowing n r region



--------------------------------------------------------------------------------
-- TOPOLOGICAL SORT ------------------------------------------------------------
--------------------------------------------------------------------------------


-- SORT DEFS


sort :: A.Region -> [Def] -> Can.Expr -> Result Can.Expr
sort region defs body =
    loop $ Graph.toSCC $ toNodes defs
  where
    loop sccs =
      case sccs of
        [] ->
          return body

        scc : rest ->
          case scc of
            Graph.Acyclic (Graph.Node _ info _) ->
              case info of
                NValue       v   -> A.At region . Can.LetValue    v   <$> loop rest
                NFunc        f   -> A.At region . Can.LetFunc     f   <$> loop rest
                NDestruct    p e -> A.At region . Can.LetDestruct p e <$> loop rest
                NDestructVar _ _ -> loop rest

            Graph.Cyclic component ->
              Graph.withComponent component $ \n ns ->
                case (,) <$> toFunc n <*> traverse toFunc ns of
                  Just (f,fs) -> A.At region . Can.LetRec f fs <$> loop rest
                  Nothing     -> throw $ badLetRec component

    toFunc (Graph.Node _ info _) =
      case info of
        NFunc        f   -> Just f
        NValue       _   -> Nothing
        NDestruct    _ _ -> Nothing
        NDestructVar _ _ -> Nothing



-- TO NODES
--
-- NDestruct/NDestructVar nodes are needed for patterns like (x,y) where cycle
-- detection is a bit trickier:
--
--   1. With patterns like (x,y), we can determine if only y is involved in the
--      cycle.
--   2. Destructures like (let _ = expr in body) that only have an NDestruct
--      node will still get through to type inference in valid topological order.
--      That way type errors are reported, even if the computation does not make
--      it all the way to code gen.
--


data NInfo
  = NValue Can.Value
  | NFunc Can.Func
  | NDestruct Can.Pattern Can.Expr
  | NDestructVar A.Region N.Name


toNodes :: [Def] -> [Graph.Node N.Name NInfo]
toNodes defs0 =
    loop 0 defs0 []
  where
    loop i defs nodes =
      case defs of
        [] ->
          nodes

        d:ds ->
          case d of
            DValue v n locals ->
              loop i ds $ Graph.Node n (NValue v) (Map.keys locals) : nodes

            DFunc f n locals ->
              loop i ds $ Graph.Node n (NFunc f) (Map.keys locals) : nodes

            DDestruct p e bindings locals ->
              let
                destruct = tempName i
              in
              loop (i + 1) ds $
                Graph.Node destruct (NDestruct p e) (Map.keys locals)
                : consWith (\n r -> Graph.Node n (NDestructVar r n) [destruct]) bindings nodes


consWith :: (k -> v -> a) -> Map.Map k v -> [a] -> [a]
consWith func dict list =
  Map.foldrWithKey (\k v xs -> func k v : xs) list dict



-- TEMP NAME


tempName :: Int -> N.Name
tempName index =
  runST (ST (\s0 ->
    let
      !(I# size) = 1 + getIndexSize index
    in
    case newByteArray# size                       s0 of { (# s1, mba #) ->
    case writeWord8Array# mba 0# 0x24#Word8 {-$-} s1 of {    s2         ->
    case writeDigitsAtEnd mba size index          s2 of {    s3         ->
    case unsafeFreezeByteArray# mba               s3 of { (# s4, ba  #) ->
      (# s4, N.fromString (S.String ba) #)
    }}}}
  ))


getIndexSize :: Int -> Int
getIndexSize n
  | n < 10    = 1
  | n < 100   = 2
  | n < 1000  = 3
  | n < 10000 = 4
  | otherwise = loop (div n 10000) 4
  where
    loop x size
      | x < 10    = size + 1
      | x < 100   = size + 2
      | x < 1000  = size + 3
      | x < 10000 = size + 4
      | otherwise = loop (div x 10000) (size + 4)


writeDigitsAtEnd :: MutableByteArray# s -> Int# -> Int -> State# s -> State# s
writeDigitsAtEnd mba oldOffset n s =
  let
    (q,r)      = quotRem n 10
    !newOffset = oldOffset -# 1#
    !(I# i)    = 0x30 + r
  in
  case writeWord8Array# mba newOffset (wordToWord8# (int2Word# i)) s of
    s1 ->
      if q <= 0
      then s1
      else writeDigitsAtEnd mba newOffset q s1



--------------------------------------------------------------------------------
-- CYCLE ERRORS ----------------------------------------------------------------
--------------------------------------------------------------------------------


-- BAD LET REC
--
-- Components full of functions are allowed, so we must shuffle
-- the nodes around such that we definitely start the component with a value.
-- That way the value will survive through to the minimal cycle.


badLetRec :: Graph.Component N.Name NInfo -> E.Problem
badLetRec component =
  Graph.withMinimalCycle valueSelector component id $ \(Value v) (Graph.MinimalCycle _ nodes) ->
    E.BadLetRec_Value v (Graph.MinimalCycle (A.toValue v) (Maybe.mapMaybe toName nodes))
  where
    toName (Graph.Node key info _) =
      case info of
        NValue       _   -> Just key
        NFunc        _   -> Just key
        NDestruct    _ _ -> Nothing
        NDestructVar _ _ -> Just key



-- VALUE SELECTOR
--
-- Cycle errors should always involve a value.


newtype Value =
  Value (A.Located N.Name)


valueSelector :: Graph.RootSelector N.Name NInfo Value
valueSelector =
  Graph.RootSelector $ \node nodes ->
    case List.sortOn toRegion (Maybe.mapMaybe toValue (node:nodes)) of
      (k,v):_ -> (k, v)
      []      -> badLetRec_CRASH node nodes
  where
    toRegion (_, Value n) =
      A.toRegion n

    toValue (Graph.Node key info _) =
      case info of
        NValue       v   -> Just (key, devalue v Value)
        NFunc        _   -> Nothing
        NDestruct    _ _ -> Nothing
        NDestructVar r n -> Just (key, Value (A.At r n))


badLetRec_CRASH :: Graph.Node N.Name NInfo -> [Graph.Node N.Name NInfo] -> a
badLetRec_CRASH node nodes =
  $(Crash.crash 'badLetRec) $
    "found bad cyclic definitions:"
    ++ concatMap (\n -> ' ' : N.toChars (Graph._key n)) (node:nodes)



--------------------------------------------------------------------------------
-- HELPERS ---------------------------------------------------------------------
--------------------------------------------------------------------------------



-- NAME HELPERS


devalue :: Can.Value -> (A.Located N.Name -> r) -> r
devalue v mk =
  case v of
    Can.Value_ n _     -> mk n
    Can.Value  n _ _ _ -> mk n



-- RESULT HELPERS


throw :: E.Problem -> Result a
throw x =
  Result $ \_ _ warns _ err ->
    err warns (OOM.one x)


throws :: OOM.OneOrMore E.Problem -> Result a
throws xs =
  Result $ \_ _ warns _ err ->
    err warns xs


instance Functor Result where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \locals tops warns ok err ->
      let
        ok' l t w arg = ok l t w (func arg)
      in
      k locals tops warns ok' err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \l t w ok _ ->
      ok l t w a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \locals tops warns ok err ->
      let
        okF ls ts ws func =
          let
            okA l t w arg = ok l t w (func arg)
          in
          kA ls ts ws okA err

        errF ws xs =
          let
            okA _ _ w _ = err w xs
            errA w ys   = err w (OOM.more xs ys)
          in
          kA locals tops ws okA errA
      in
      kF locals tops warns okF errF


instance Monad Result where
  {-# INLINE (>>=) #-}
  (>>=) (Result kA) callback =
    Result $ \locals tops warns ok err ->
      let
        okA l t w a =
          case callback a of
            Result kB -> kB l t w ok err
      in
      kA locals tops warns okA err
