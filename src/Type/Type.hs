module Type.Type where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import Type.PrettyPrint
import Text.PrettyPrint as P
import System.IO.Unsafe
import Control.Applicative ((<$>),(<*>))
import Control.Monad.State (StateT, get, put, execStateT, runStateT)
import Control.Monad.Error (ErrorT, Error, liftIO)
import Data.Traversable (traverse)
import AST.Annotation
import qualified AST.PrettyPrint as PP
import qualified AST.Type as T
import qualified AST.Variable as Var


data Term1 a
    = App1 a a
    | Fun1 a a
    | Var1 a
    | EmptyRecord1
    | Record1 (Map.Map String [a]) a


data TermN a
    = VarN  (Maybe Var.Canonical) a
    | TermN (Maybe Var.Canonical) (Term1 (TermN a))


varN :: a -> TermN a
varN = VarN Nothing


termN :: (Term1 (TermN a)) -> TermN a
termN = TermN Nothing


record :: Map.Map String [TermN a] -> TermN a -> TermN a
record fs rec = termN (Record1 fs rec)


type Type = TermN Variable


type Variable = UF.Point Descriptor


type SchemeName = String


type TypeName = Var.Canonical


type Constraint a b = Located (BasicConstraint a b)


data BasicConstraint a b
    = CTrue
    | CSaveEnv
    | CEqual a a
    | CAnd [Constraint a b]
    | CLet [Scheme a b] (Constraint a b)
    | CInstance SchemeName a


data Scheme a b = Scheme
    { rigidQuantifiers :: [b]
    , flexibleQuantifiers :: [b]
    , constraint :: Constraint a b
    , header :: Map.Map String a
    }


type TypeConstraint = Constraint Type Variable

type TypeScheme = Scheme Type Variable


monoscheme :: Map.Map String a -> Scheme a b
monoscheme headers = Scheme [] [] (noneNoDocs CTrue) headers


infixl 8 /\

(/\) :: Constraint a b -> Constraint a b -> Constraint a b
a@(A _ c1) /\ b@(A _ c2) =
    case (c1, c2) of
      (CTrue, _) -> b
      (_, CTrue) -> a
      _ -> mergeOldDocs a b (CAnd [a,b])


infixr 9 ==>

(==>) :: Type -> Type -> Type
a ==> b = termN (Fun1 a b)


(<|) :: TermN a -> TermN a -> TermN a
f <| a = termN (App1 f a)


data Descriptor = Descriptor
    { structure :: Maybe (Term1 Variable)
    , rank :: Int
    , flex :: Flex
    , name :: Maybe TypeName
    , copy :: Maybe Variable
    , mark :: Int
    , alias :: Maybe Var.Canonical
    }


noRank :: Int
noRank = -1


outermostRank :: Int
outermostRank = 0


noMark :: Int
noMark = 0


initialMark :: Int
initialMark = 1


data Flex
    = Rigid
    | Flexible
    | Constant
    | Is SuperType
    deriving (Eq)


data SuperType
    = Number
    | Comparable
    | Appendable
    deriving (Eq)


namedVar :: Flex -> Var.Canonical -> IO Variable
namedVar flex name = UF.fresh $ Descriptor
  { structure = Nothing
  , rank = noRank
  , flex = flex
  , name = Just name
  , copy = Nothing
  , mark = noMark
  , alias = Nothing
  }


variable :: Flex -> IO Variable
variable flex = UF.fresh $ Descriptor
  { structure = Nothing
  , rank = noRank
  , flex = flex
  , name = Nothing
  , copy = Nothing
  , mark = noMark
  , alias = Nothing
  }


-- ex qs constraint == exists qs. constraint
ex :: [Variable] -> TypeConstraint -> TypeConstraint
ex fqs constraint@(A ann _) =
    A ann $ CLet [Scheme [] fqs constraint Map.empty] (A ann CTrue)


-- fl qs constraint == forall qs. constraint
fl :: [Variable] -> TypeConstraint -> TypeConstraint
fl rqs constraint@(A ann _) =
    A ann $ CLet [Scheme rqs [] constraint Map.empty] (A ann CTrue)


exists :: Error e => (Type -> ErrorT e IO TypeConstraint) -> ErrorT e IO TypeConstraint
exists f =
  do  v <- liftIO $ variable Flexible
      ex [v] <$> f (varN v)


existsNumber :: Error e => (Type -> ErrorT e IO TypeConstraint) -> ErrorT e IO TypeConstraint
existsNumber f =
  do  v <- liftIO $ variable (Is Number)
      ex [v] <$> f (varN v)


-- TYPES TO PRETTY STRINGS

instance PrettyType a => PrettyType (UF.Point a) where
  pretty when point =
      unsafePerformIO $ fmap (pretty when) (UF.descriptor point)


instance PrettyType t => PrettyType (Annotated a t) where
  pretty when (A _ e) =
      pretty when e


instance PrettyType a => PrettyType (Term1 a) where
  pretty when term =
    let prty = pretty Never in
    case term of
      App1 f x ->
          parensIf needed (px <+> pretty App x)
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
          P.braces (extend <+> commaSep prettyFields)
        where
          prettyExt = prty ext
          extend | P.render prettyExt == "{}" = P.empty
                 | otherwise = prettyExt <+> P.text "|"
          mkPretty f t = P.text f <+> P.text ":" <+> prty t
          prettyFields = concatMap (\(f,ts) -> map (mkPretty f) ts) (Map.toList fields)


instance PrettyType a => PrettyType (TermN a) where
  pretty when term =
    case term of
      VarN alias x -> either alias (pretty when x)
      TermN alias t1 -> either alias (pretty when t1)
    where
      either maybeAlias doc =
          case maybeAlias of
            Just alias -> PP.pretty alias
            Nothing -> doc


instance PrettyType Descriptor where
  pretty when desc = do
    case (alias desc, structure desc, name desc) of
      (Just name, _, _) -> PP.pretty name
      (_, Just term, _) -> pretty when term
      (_, _, Just name)
          | Var.isTuple name ->
              P.parens . P.text $ replicate (read (drop 6 (Var.toString name)) - 1) ','
          | otherwise ->
              P.text (Var.toString name)
                            
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

      CLet [Scheme [] fqs constraint header] (A _ CTrue) | Map.null header ->
          P.sep [ binder, pretty Never c ]
        where
          mergeExists vs (A _ c) =
            case c of
              CLet [Scheme [] fqs' c' _] (A _ CTrue) -> mergeExists (vs ++ fqs') c'
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
  pretty _ (Scheme rqs fqs (A _ constraint) headers) =
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


-- ADDING NAMES TO TYPES

addNames :: (Crawl t) => t -> IO t
addNames value = do
    (rawVars, _, _, _) <- execStateT (crawl getNames value) ([], 0, 0, 0)
    let vars = map head . List.group $ List.sort rawVars
        suffix s = map (++s) (map (:[]) ['a'..'z'])
        allVars = concatMap suffix $ ["","'","_"] ++ map show [ (0 :: Int) .. ]
        okayVars = filter (`notElem` vars) allVars
    runStateT (crawl rename value) (okayVars, 0, 0, 0)
    return value
  where
    getNames desc state@(vars, a, b, c) =
        let name' = name desc in
        case name' of
          Just (Var.Canonical _ var) -> (name', (var:vars, a, b, c))
          _ -> (name', state)

    rename desc state@(vars, a, b, c) =
      case name desc of
        Just var -> (Just var, state)
        Nothing ->
            case flex desc of
              Is Number     -> (local $ "number"     ++ replicate a '\'', (vars, a+1, b, c))
              Is Comparable -> (local $ "comparable" ++ replicate b '\'', (vars, a, b+1, c))
              Is Appendable -> (local $ "appendable" ++ replicate c '\'', (vars, a, b, c+1))
              _             -> (local $ head vars, (tail vars, a, b, c))
            where
              local v = Just (Var.local v)


-- CRAWLING OVER TYPES

type CrawlState = ([String], Int, Int, Int)


-- Code for traversing all the type data-structures and giving
-- names to the variables embedded deep in there.
class Crawl t where
  crawl :: (Descriptor -> CrawlState -> (Maybe TypeName, CrawlState))
        -> t
        -> StateT CrawlState IO t


instance Crawl e => Crawl (Annotated a e) where
  crawl nextState (A ann e) = A ann <$> crawl nextState e


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
    let rnm = crawl nextState
    in
        Scheme
            <$> rnm rqs
            <*> rnm fqs
            <*> crawl nextState c
            <*> return headers


instance Crawl t => Crawl (TermN t) where
  crawl nextState tipe =
    case tipe of
      VarN a x ->
          VarN a <$> crawl nextState x

      TermN a term ->
          TermN a <$> crawl nextState term


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
  crawl nextState point =
    do  desc <- liftIO $ UF.descriptor point
        desc' <- crawl nextState desc
        liftIO $ UF.setDescriptor point desc'
        return point


instance Crawl Descriptor where
  crawl nextState desc =
    do  state <- get
        let (name', state') = nextState desc state
        structure' <- traverse (crawl nextState) (structure desc)
        put state'
        return $ desc { name = name', structure = structure' }

               
toSrcType :: Variable -> IO T.CanonicalType
toSrcType var =
    go =<< addNames var
  where
    go v =
      do  desc <- UF.descriptor v
          srcType <- maybe (backupSrcType desc) termToSrcType (structure desc)
          return $ maybe srcType (\name -> T.Aliased name srcType) (alias desc)

    backupSrcType desc = 
        case name desc of
          Just v@(Var.Canonical _ x@(c:_))
              | Char.isLower c -> return (T.Var x)
              | otherwise -> return $ T.Type v

          _ -> error $ concat
                        [ "Problem converting the following type "
                        , "from a type-checker type to a source-syntax type:"
                        , P.render (pretty Never var) ]

    termToSrcType term =
        case term of
          App1 a b -> do
            a' <- go a
            b' <- go b
            case a' of
              T.App f args -> return $ T.App f (args ++ [b'])
              _            -> return $ T.App a' [b']

          Fun1 a b -> T.Lambda <$> go a <*> go b

          Var1 a -> go a

          EmptyRecord1 -> return $ T.Record [] Nothing

          Record1 tfields extension -> do
            fields' <- traverse (mapM go) tfields
            let fields = concat [ map ((,) name) ts | (name,ts) <- Map.toList fields' ]
            ext' <- dealias <$> go extension
            return $ case ext' of
                       T.Record fs ext -> T.Record (fs ++ fields) ext
                       T.Var _ -> T.Record fields (Just ext')
                       _ -> error "Used toSrcType on a type that is not well-formed"

    dealias :: T.CanonicalType -> T.CanonicalType
    dealias t =
        case t of
          T.Aliased _ t' -> t'
          _ -> t


data AppStructure = List Variable | Tuple [Variable] | Other


collectApps :: Variable -> IO AppStructure
collectApps var = go [] var
  where
    go vars var = do
      desc <- UF.descriptor var
      case (structure desc, vars) of
        (Nothing, [v]) ->
            case name desc of
              Just n | Var.isList n -> return (List v)
              _ -> return Other

        (Nothing,  vs) ->
            case name desc of
              Just n | Var.isTuple n -> return (Tuple vs)
              _ -> return Other

        (Just term, _) ->
            case term of
              App1 a b -> go (vars ++ [b]) a
              _ -> return Other
