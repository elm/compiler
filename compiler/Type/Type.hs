module Type.Type where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import Type.PrettyPrint
import Text.PrettyPrint as P
import System.IO.Unsafe
import Control.Applicative ((<$>),(<*>))
import Control.Monad.State
import Data.Traversable (traverse)
import SourceSyntax.Helpers (isTuple)

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

record fs rec = TermN (Record1 fs rec)

type SchemeName = String
type TypeName = String

data Constraint a b
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

monoscheme headers = Scheme [] [] CTrue headers

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

data Flex = Rigid | Flexible | Constant
     deriving (Show, Eq)

type Variable = UF.Point Descriptor

type Type = TermN Variable
type TypeConstraint = Constraint Type Variable
type TypeScheme = Scheme Type Variable

infixl 8 /\

(/\) :: Constraint a b -> Constraint a b -> Constraint a b
a /\ CTrue = a
CTrue /\ b = b
a /\ b = CAnd [a,b]

(===) :: Type -> Type -> TypeConstraint
(===) = CEqual

(<?) :: SchemeName -> Type -> TypeConstraint
x <? t = CInstance x t

infixr 9 ==>
(==>) :: Type -> Type -> Type
a ==> b = TermN (Fun1 a b)

f <| a = TermN (App1 f a)


namedVar flex name = UF.fresh $ Descriptor {
    structure = Nothing,
    rank = noRank,
    flex = flex,
    name = Just name,
    copy = Nothing,
    mark = noMark
  }

flexibleVar = UF.fresh $ Descriptor {
    structure = Nothing,
    rank = noRank,
    flex = Flexible,
    name = Nothing,
    copy = Nothing,
    mark = noMark
  }

rigidVar = UF.fresh $ Descriptor {
    structure = Nothing,
    rank = noRank,
    flex = Rigid,
    name = Nothing,
    copy = Nothing,
    mark = noMark
  }

-- ex qs constraint == exists qs. constraint
ex :: [Variable] -> TypeConstraint -> TypeConstraint
ex fqs constraint = CLet [Scheme [] fqs constraint Map.empty] CTrue

-- fl qs constraint == forall qs. constraint
fl :: [Variable] -> TypeConstraint -> TypeConstraint
fl rqs constraint = CLet [Scheme rqs [] constraint Map.empty] CTrue

exists :: (Type -> IO TypeConstraint) -> IO TypeConstraint
exists f = do
  v <- flexibleVar
  ex [v] <$> f (VarN v)


instance Show a => Show (UF.Point a) where
  show point = unsafePerformIO $ fmap show (UF.descriptor point)


instance PrettyType a => PrettyType (UF.Point a) where
  pretty when point = unsafePerformIO $ fmap (pretty when) (UF.descriptor point)


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

      Record1 fields ext ->
          P.braces (prty ext <+> P.text "|" <+> commaSep prettyFields)
        where
          mkPretty f t = P.text f <+> P.text ":" <+> prty t
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
      (_, Just name) -> if not (isTuple name) then P.text name else
                            P.parens . P.text $ replicate (read (drop 6 name) - 1) ','
      _ -> P.text "?"


instance (PrettyType a, PrettyType b) => PrettyType (Constraint a b) where
  pretty _ constraint =
    let prty = pretty Never in
    case constraint of
      CTrue -> P.text "True"
      CSaveEnv -> P.text "SaveTheEnvironment!!!"
      CEqual a b -> prty a <+> P.text "=" <+> prty b
      CAnd [] -> P.text "True"

      CAnd cs ->
        P.parens . P.sep $ P.punctuate (P.text " and") (map (pretty Never) cs)

      CLet [Scheme [] fqs constraint header] CTrue | Map.null header ->
          P.sep [ binder, pretty Never c ]
        where
          mergeExists vs c =
            case c of
              CLet [Scheme [] fqs' c' _] CTrue -> mergeExists (vs ++ fqs') c'
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
  pretty _ (Scheme rqs fqs constraint headers) =
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
extraPretty value = do
    (_, rawVars) <- runStateT (crawl getNames value) []
    let vars = map head . List.group $ List.sort rawVars
        suffix s = map (++s) (map (:[]) ['a'..'z'])
        allVars = concatMap suffix $ ["","'","_"] ++ map show [0..]
        okayVars = filter (`notElem` vars) allVars
    runStateT (crawl rename value) okayVars
    return (pretty Never value)
  where
    getNames name vars =
      case name of
        Just var -> (name, var:vars)
        Nothing -> (name, vars)

    rename name vars =
      case name of
        Just var -> (name, vars)
        Nothing -> (Just (head vars), tail vars)
--}

-- Code for traversing all the type data-structures and giving
-- names to the variables embedded deep in there.
class Crawl t where
  crawl :: (Maybe TypeName -> [String] -> (Maybe TypeName, [String]))
        -> t
        -> StateT [String] IO t

instance (Crawl t, Crawl v) => Crawl (Constraint t v) where
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
    let (name', state') = nextState (name desc) state
    structure' <- traverse (crawl nextState) (structure desc)
    put state'
    return $ desc { name = name', structure = structure' }
