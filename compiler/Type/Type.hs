module Type.Type where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import Type.PrettyPrint
import Text.PrettyPrint as P
import System.IO.Unsafe
import Control.Applicative ((<$>),(<*>))
import Control.Monad.State
import Control.Monad.Error
import Data.Traversable (traverse)
import SourceSyntax.Location
import SourceSyntax.Helpers (isTuple)
import qualified SourceSyntax.Type as Src

data Term1 a
    = App1 a a
    | Fun1 a a
    | Var1 a
    | EmptyRecord1
    | Record1 (Map.Map String [a]) a
    deriving Show

data TermN a
    = VarN a
    | TermN (Term1 (TermN a))
    deriving Show

record :: Map.Map String [TermN a] -> TermN a -> TermN a
record fs rec = TermN (Record1 fs rec)

type Type = TermN Variable
type Variable = UF.Point Descriptor

type SchemeName = String
type TypeName = String

type Constraint a b = Located (BasicConstraint a b)
data BasicConstraint a b
    = CTrue
    | CSaveEnv
    | CEqual a a
    | CAnd [Constraint a b]
    | CLet [Scheme a b] (Constraint a b)
    | CInstance SchemeName a
    deriving Show

data Scheme a b = Scheme {
    rigidQuantifiers :: [b],
    flexibleQuantifiers :: [b],
    constraint :: Constraint a b,
    header :: Map.Map String a
} deriving Show

type TypeConstraint = Constraint Type Variable
type TypeScheme = Scheme Type Variable

monoscheme headers = Scheme [] [] (noneNoDocs CTrue) headers

infixl 8 /\

(/\) :: Constraint a b -> Constraint a b -> Constraint a b
a@(L _ c1) /\ b@(L _ c2) =
    case (c1, c2) of
      (CTrue, _) -> b
      (_, CTrue) -> a
      _ -> mergeOldDocs a b (CAnd [a,b])

infixr 9 ==>
(==>) :: Type -> Type -> Type
a ==> b = TermN (Fun1 a b)

f <| a = TermN (App1 f a)

data Descriptor = Descriptor {
    structure :: Maybe (Term1 Variable),
    rank :: Int,
    flex :: Flex,
    name :: Maybe TypeName,
    copy :: Maybe Variable,
    mark :: Int
} deriving Show

noRank = -1
outermostRank = 0 :: Int

noMark = 0
initialMark = 1

data Flex = Rigid | Flexible | Constant | Is SuperType
     deriving (Show, Eq)

data SuperType = Number | Comparable | Appendable
     deriving (Show, Eq)

namedVar :: Flex -> String -> IO Variable
namedVar flex name = UF.fresh $ Descriptor {
    structure = Nothing,
    rank = noRank,
    flex = flex,
    name = Just name,
    copy = Nothing,
    mark = noMark
  }

var :: Flex -> IO Variable
var flex = UF.fresh $ Descriptor {
    structure = Nothing,
    rank = noRank,
    flex = flex,
    name = Nothing,
    copy = Nothing,
    mark = noMark
  }

structuredVar :: Term1 Variable -> IO Variable
structuredVar structure = UF.fresh $ Descriptor {
    structure = Just structure,
    rank = noRank,
    flex = Flexible,
    name = Nothing,
    copy = Nothing,
    mark = noMark
  }


-- ex qs constraint == exists qs. constraint
ex :: [Variable] -> TypeConstraint -> TypeConstraint
ex fqs constraint@(L s _) = L s $ CLet [Scheme [] fqs constraint Map.empty] (L s CTrue)

-- fl qs constraint == forall qs. constraint
fl :: [Variable] -> TypeConstraint -> TypeConstraint
fl rqs constraint@(L s _) = L s $ CLet [Scheme rqs [] constraint Map.empty] (L s CTrue)

exists :: Error e => (Type -> ErrorT e IO TypeConstraint) -> ErrorT e IO TypeConstraint
exists f = do
  v <- liftIO $ var Flexible
  ex [v] <$> f (VarN v)


instance Show a => Show (UF.Point a) where
  show point = unsafePerformIO $ fmap show (UF.descriptor point)


instance PrettyType a => PrettyType (UF.Point a) where
  pretty when point = unsafePerformIO $ fmap (pretty when) (UF.descriptor point)


instance PrettyType a => PrettyType (Located a) where
  pretty when (L _ e) = pretty when e


instance PrettyType a => PrettyType (Term1 a) where
  pretty when term =
    let prty = pretty Never in
    case term of
      App1 f x | P.render px == "_List" -> P.brackets (pretty Never x)
               | otherwise -> parensIf needed (px <+> pretty App x)
        where
          px = prty f
          needed = case when of
                     App -> True
                     _ -> False

      Fun1 arg body ->
          parensIf needed (pretty Fn arg <+> P.text "->" <+> prty body)
        where
          needed = case when of
                     Never -> False
                     _ -> True

      Var1 x -> prty x

      EmptyRecord1 -> P.braces P.empty

      Record1 fields ext -> P.braces (extend <+> commaSep prettyFields)
        where
          prettyExt = prty ext
          extend | P.render prettyExt == "{}" = P.empty
                 | otherwise = prettyExt <+> P.text "|"
          mkPretty f t = P.text (reprime f) <+> P.text ":" <+> prty t
          prettyFields = concatMap (\(f,ts) -> map (mkPretty f) ts) (Map.toList fields)


instance PrettyType a => PrettyType (TermN a) where
  pretty when term =
    case term of
      VarN x -> pretty when x
      TermN t1 -> pretty when t1


instance PrettyType Descriptor where
  pretty when desc =
    case (structure desc, name desc) of
      (Just term, _) -> pretty when term
      (_, Just name) -> if not (isTuple name) then P.text (reprime name) else
                            P.parens . P.text $ replicate (read (drop 6 name) - 1) ','
      _ -> P.text "?"


instance (PrettyType a, PrettyType b) => PrettyType (BasicConstraint a b) where
  pretty _ constraint =
    let prty = pretty Never in
    case constraint of
      CTrue -> P.text "True"
      CSaveEnv -> P.text "SaveTheEnvironment!!!"
      CEqual a b -> prty a <+> P.text "=" <+> prty b
      CAnd [] -> P.text "True"

      CAnd cs ->
        P.parens . P.sep $ P.punctuate (P.text " and") (map (pretty Never) cs)

      CLet [Scheme [] fqs constraint header] (L _ CTrue) | Map.null header ->
          P.sep [ binder, pretty Never c ]
        where
          mergeExists vs (L _ c) =
            case c of
              CLet [Scheme [] fqs' c' _] (L _ CTrue) -> mergeExists (vs ++ fqs') c'
              _ -> (vs, c)

          (fqs', c) = mergeExists fqs constraint

          binder = if null fqs' then P.empty else
                     P.text "\x2203" <+> P.hsep (map (pretty Never) fqs') <> P.text "."

      CLet schemes constraint ->
        P.fsep [ P.hang (P.text "let") 4 (P.brackets . commaSep $ map (pretty Never) schemes)
               , P.text "in", pretty Never constraint ]

      CInstance name tipe ->
        P.text name <+> P.text "<" <+> prty tipe

instance (PrettyType a, PrettyType b) => PrettyType (Scheme a b) where
  pretty _ (Scheme rqs fqs (L _ constraint) headers) =
      P.sep [ forall, cs, headers' ]
    where
      prty = pretty Never

      forall = if null rqs && null fqs then P.empty else
               P.text "\x2200" <+> frees <+> rigids

      frees = P.hsep $ map prty fqs
      rigids = if null rqs then P.empty else P.braces . P.hsep $ map prty rqs

      cs = case constraint of
             CTrue -> P.empty
             CAnd [] -> P.empty
             _ -> P.brackets (pretty Never constraint)

      headers' = if Map.size headers > 0 then dict else P.empty
      dict = P.parens . commaSep . map prettyPair $ Map.toList headers
      prettyPair (n,t) = P.text n <+> P.text ":" <+> pretty Never t


extraPretty :: (PrettyType t, Crawl t) => t -> IO Doc
extraPretty t = pretty Never <$> addNames t

addNames :: (Crawl t) => t -> IO t
addNames value = do
    (rawVars, _, _, _) <- execStateT (crawl getNames value) ([], 0, 0, 0)
    let vars = map head . List.group $ List.sort rawVars
        suffix s = map (++s) (map (:[]) ['a'..'z'])
        allVars = concatMap suffix $ ["","'","_"] ++ map show [0..]
        okayVars = filter (`notElem` vars) allVars
    runStateT (crawl rename value) (okayVars, 0, 0, 0)
    return value
  where
    getNames desc state@(vars, a, b, c) =
        let name' = name desc in
        case name' of
          Just var -> (name', (var:vars, a, b, c))
          Nothing -> (name', state)

    rename desc state@(vars, a, b, c) =
      case name desc of
        Just var -> (Just var, state)
        Nothing ->
            case flex desc of
              Is Number     -> (Just $ "number"     ++ replicate a '\'', (vars, a+1, b, c))
              Is Comparable -> (Just $ "comparable" ++ replicate b '\'', (vars, a, b+1, c))
              Is Appendable -> (Just $ "appendable" ++ replicate c '\'', (vars, a, b, c+1))
              other         -> (Just $ head vars, (tail vars, a, b, c))
                  where mark = case other of
                                 Flexible -> ""
                                 Rigid    -> "!"
                                 Constant -> "#"


type CrawlState = ([String], Int, Int, Int)

-- Code for traversing all the type data-structures and giving
-- names to the variables embedded deep in there.
class Crawl t where
  crawl :: (Descriptor -> CrawlState -> (Maybe TypeName, CrawlState))
        -> t
        -> StateT CrawlState IO t

instance Crawl a => Crawl (Located a) where
  crawl nextState (L s e) = L s <$> crawl nextState e

instance (Crawl t, Crawl v) => Crawl (BasicConstraint t v) where
  crawl nextState constraint = 
    let rnm = crawl nextState in
    case constraint of
      CTrue -> return CTrue
      CSaveEnv -> return CSaveEnv
      CEqual a b -> CEqual <$> rnm a <*> rnm b
      CAnd cs -> CAnd <$> crawl nextState cs
      CLet schemes c -> CLet <$> crawl nextState schemes <*> crawl nextState c 
      CInstance name tipe -> CInstance name <$> rnm tipe

instance Crawl a => Crawl [a] where
  crawl nextState list = mapM (crawl nextState) list

instance (Crawl t, Crawl v) => Crawl (Scheme t v) where
  crawl nextState (Scheme rqs fqs c headers) =
    let rnm = crawl nextState in
    Scheme <$> rnm rqs <*> rnm fqs <*> crawl nextState c <*> return headers

instance Crawl t => Crawl (TermN t) where
  crawl nextState tipe =
    case tipe of
      VarN x -> VarN <$> crawl nextState x
      TermN term -> TermN <$> crawl nextState term

instance Crawl t => Crawl (Term1 t) where
  crawl nextState term =
     let rnm = crawl nextState in
     case term of
      App1 a b -> App1 <$> rnm a <*> rnm b
      Fun1 a b -> Fun1 <$> rnm a <*> rnm b
      Var1 a -> Var1 <$> rnm a
      EmptyRecord1 -> return EmptyRecord1
      Record1 fields ext ->
          Record1 <$> traverse (mapM rnm) fields <*> rnm ext

instance Crawl a => Crawl (UF.Point a) where
  crawl nextState point = do
    desc <- liftIO $ UF.descriptor point
    desc' <- crawl nextState desc
    liftIO $ UF.setDescriptor point desc'
    return point

instance Crawl Descriptor where
  crawl nextState desc = do
    state <- get
    let (name', state') = nextState desc state
    structure' <- traverse (crawl nextState) (structure desc)
    put state'
    return $ desc { name = name', structure = structure' }

               
toSrcType :: Variable -> IO Src.Type
toSrcType variable = do
  desc <- UF.descriptor =<< addNames variable
  case structure desc of
    Just term ->
        case term of
          App1 a b -> do
            Src.Data name ts <- toSrcType a
            b' <- toSrcType b
            return (Src.Data name (ts ++ [b']))
          Fun1 a b -> Src.Lambda <$> toSrcType a <*> toSrcType b
          Var1 a -> toSrcType a
          EmptyRecord1 -> return Src.EmptyRecord
          Record1 fs ext -> do
            fs' <- traverse (mapM toSrcType) fs
            let fs'' = concat [ map ((,) name) ts | (name,ts) <- Map.toList fs' ]
            Src.Record fs'' <$> toSrcType ext
    Nothing ->
        case name desc of
          Just x@(c:_) | Char.isLower c -> return (Src.Var x)
                       | otherwise      -> return (Src.Data x [])
          _ -> error $ concat
                        [ "Problem converting the following type "
                        , "from a type-checker type to a source-syntax type:"
                        , P.render (pretty Never variable) ]

data AppStructure = List Variable | Tuple [Variable] | Other

collectApps :: Variable -> IO AppStructure
collectApps variable = go [] variable
  where
    go vars variable = do
      desc <- UF.descriptor variable
      case (structure desc, vars) of
        (Nothing, [v]) -> case name desc of
                             Just "_List" -> return (List v)
                             _ -> return Other
        (Nothing,  vs) -> case name desc of
                             Just ctor | isTuple ctor -> return (Tuple vs)
                             _ -> return Other
        (Just term, _) -> case term of
                            App1 a b -> go (vars ++ [b]) a
                            _ -> return Other
