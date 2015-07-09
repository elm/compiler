module Type.Type where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Traverse (traverse)
import qualified Data.UnionFind.IO as UF
import Text.PrettyPrint as P
import System.IO.Unsafe

import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.PrettyPrint


-- CONCRETE TYPES

type Type = TermN Variable

type Variable = UF.Point Descriptor

type TypeConstraint = Constraint Type Variable

type TypeScheme = Scheme Type Variable

type Alias a = Maybe (Var.Canonical, [(String,a)])


-- TYPE PRIMITIVES

data Term1 a
    = App1 a a
    | Fun1 a a
    | Var1 a
    | EmptyRecord1
    | Record1 (Map.Map String [a]) a


data TermN a
    = PlaceHolder String
    | VarN (Alias (TermN a)) a
    | TermN (Alias (TermN a)) (Term1 (TermN a))


varN :: a -> TermN a
varN =
  VarN Nothing


termN :: (Term1 (TermN a)) -> TermN a
termN =
  TermN Nothing


record :: Map.Map String [TermN a] -> TermN a -> TermN a
record fs rec =
  termN (Record1 fs rec)


-- DESCRIPTORS

data Descriptor = Descriptor
    { structure :: Maybe (Term1 Variable)
    , rank :: Int
    , flex :: Flex
    , name :: Maybe TypeName
    , copy :: Maybe Variable
    , mark :: Int
    , alias :: Alias Variable
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
    | Error
    deriving (Eq)

data SuperType
    = Number
    | Comparable
    | Appendable
    deriving (Eq)


-- CONSTRAINTS

data Constraint a b
    = CTrue
    | CSaveEnv
    | CEqual Error.Hint R.Region a a
    | CAnd [Constraint a b]
    | CLet [Scheme a b] (Constraint a b)
    | CInstance R.Region SchemeName a

type SchemeName = String

type TypeName = Var.Canonical

data Scheme a b = Scheme
    { rigidQuantifiers :: [b]
    , flexibleQuantifiers :: [b]
    , constraint :: Constraint a b
    , header :: Map.Map String (A.Located a)
    }


-- TYPE HELPERS

infixr 9 ==>

(==>) :: Type -> Type -> Type
(==>) a b =
  termN (Fun1 a b)


(<|) :: TermN a -> TermN a -> TermN a
(<|) f a =
  termN (App1 f a)


-- VARIABLE HELPERS

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
variable flex =
  UF.fresh (descriptor flex)


descriptor :: Flex -> Descriptor
descriptor flex =
  Descriptor
    { structure = Nothing
    , rank = noRank
    , flex = flex
    , name = Nothing
    , copy = Nothing
    , mark = noMark
    , alias = Nothing
    }


-- CONSTRAINT HELPERS

monoscheme :: Map.Map String (A.Located a) -> Scheme a b
monoscheme headers =
  Scheme [] [] CTrue headers


infixl 8 /\

(/\) :: Constraint a b -> Constraint a b -> Constraint a b
(/\) c1 c2 =
    case (c1, c2) of
      (CTrue, _) -> c2
      (_, CTrue) -> c1
      _ -> CAnd [c1,c2]


-- ex qs constraint == exists qs. constraint
ex :: [Variable] -> TypeConstraint -> TypeConstraint
ex fqs constraint =
    CLet [Scheme [] fqs constraint Map.empty] CTrue


-- fl qs constraint == forall qs. constraint
fl :: [Variable] -> TypeConstraint -> TypeConstraint
fl rqs constraint =
    CLet [Scheme rqs [] constraint Map.empty] CTrue


exists :: (Type -> IO TypeConstraint) -> IO TypeConstraint
exists f =
  do  v <- variable Flexible
      ex [v] <$> f (varN v)


existsNumber :: (Type -> IO TypeConstraint) -> IO TypeConstraint
existsNumber f =
  do  v <- variable (Is Number)
      ex [v] <$> f (varN v)


-- TYPES TO PRETTY STRINGS

instance PrettyType a => PrettyType (UF.Point a) where
  pretty when point =
      unsafePerformIO $ fmap (pretty when) (UF.descriptor point)


instance PrettyType a => PrettyType (Term1 a) where
  pretty when term =
    let prty = pretty Never in
    case term of
      App1 f x ->
          parensIf needed (px <+> pretty App x)
        where
          px = prty f
          needed =
            case when of
              App -> True
              _ -> False

      Fun1 arg body ->
          parensIf needed (pretty Fn arg <+> P.text "->" <+> prty body)
        where
          needed =
            case when of
              Never -> False
              _ -> True

      Var1 x ->
          prty x

      EmptyRecord1 ->
          P.braces P.empty

      Record1 fields ext ->
          P.braces (extend <+> commaSep prettyFields)
        where
          prettyExt = prty ext

          extend
            | P.render prettyExt == "{}" =
                P.empty
            | otherwise =
                prettyExt <+> P.text "|"

          mkPretty f t =
            P.text f <+> P.text ":" <+> prty t

          prettyFields =
            concatMap (\(f,ts) -> map (mkPretty f) ts) (Map.toList fields)


instance PrettyType a => PrettyType (TermN a) where
  pretty when term =
    case term of
      PlaceHolder _ ->
          error "problem prettifying type, probably indicates a bigger problem"

      VarN alias x ->
          either alias (pretty when x)

      TermN alias t1 ->
          either alias (pretty when t1)
    where
      either maybeAlias doc =
          case maybeAlias of
            Nothing -> doc
            Just (name, args) ->
                P.hang
                    (P.text (Var.toString name))
                    2
                    (P.sep (map (pretty App . snd) args))


instance PrettyType Descriptor where
  pretty when desc =
    case (alias desc, structure desc, name desc) of
      (Just (name, args), _, _) ->
          P.hang
              (P.text (Var.toString name))
              4
              (P.sep (map (pretty App . snd) args))

      (_, Just term, _) ->
          pretty when term

      (_, _, Just name)
          | Var.isTuple name ->
              P.parens . P.text $ replicate (read (drop 6 (Var.toString name)) - 1) ','
          | otherwise ->
              P.text (Var.toString name)

      _ -> P.text "?"


instance (PrettyType a, PrettyType b) => PrettyType (Constraint a b) where
  pretty _ constraint =
    let prty = pretty Never in
    case constraint of
      CTrue ->
          P.text "True"

      CSaveEnv ->
          P.text "SaveTheEnvironment!!!"

      CEqual _ _ a b ->
          prty a <+> P.text "=" <+> prty b

      CAnd [] ->
          P.text "True"

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

          binder =
              if null fqs'
                then P.empty
                else
                  P.text "\x2203" <+> P.hsep (map (pretty Never) fqs') <> P.text "."

      CLet schemes constraint ->
          P.fsep
            [ P.hang (P.text "let") 4 (P.brackets . commaSep $ map (pretty Never) schemes)
            , P.text "in", pretty Never constraint
            ]

      CInstance _region name tipe ->
          P.text name <+> P.text "<" <+> prty tipe


instance (PrettyType a, PrettyType b) => PrettyType (Scheme a b) where
  pretty _ (Scheme rqs fqs constraint headers) =
      P.sep [ forall, cs, headers' ]
    where
      prty = pretty Never

      forall =
          if null rqs && null fqs
            then P.empty
            else P.text "\x2200" <+> frees <+> rigids

      frees = P.hsep (map prty fqs)

      rigids =
          if null rqs
            then P.empty
            else P.braces . P.hsep $ map prty rqs

      cs =
          case constraint of
            CTrue -> P.empty
            CAnd [] -> P.empty
            _ -> P.brackets (pretty Never constraint)

      headers' =
          if Map.size headers > 0 then dict else P.empty

      dict =
          P.parens . commaSep . map prettyPair $ Map.toList headers

      prettyPair (fieldName, A.A _ tipe) =
          P.text fieldName <+> P.text ":" <+> pretty Never tipe


-- CONVERT TO SOURCE TYPES

-- TODO: Attach resulting type to the descriptor so that you
-- never have to do extra work, particularly nice for aliased types
toSrcType :: Variable -> IO T.Canonical
toSrcType variable =
  do  addNames variable
      variableToSrcType variable variable


variableToSrcType :: Variable -> Variable -> IO T.Canonical
variableToSrcType rootVariable variable =
  do  desc <- UF.descriptor variable
      srcType <- maybe (backupSrcType desc) (termToSrcType rootVariable) (structure desc)
      case alias desc of
        Nothing ->
            return srcType

        Just (name, args) ->
            case srcType of
              T.Type (Var.Canonical Var.BuiltIn _) ->
                  return srcType

              _ ->
                  do  args' <- mapM (\(arg,tvar) -> (,) arg <$> variableToSrcType rootVariable tvar) args
                      return (T.Aliased name args' (T.Filled srcType))
  where
    backupSrcType :: Descriptor -> IO T.Canonical
    backupSrcType desc =
        case name desc of
          Just v@(Var.Canonical _ x@(c:_))
              | Char.isLower c ->
                  return (T.Var x)
              | otherwise ->
                  return $ T.Type v

          _ ->
              error $
                concat
                  [ "Problem converting the following type "
                  , "from a type-checker type to a source-syntax type:"
                  , P.render (pretty Never rootVariable)
                  ]


termToSrcType :: Variable -> Term1 Variable -> IO T.Canonical
termToSrcType rootVariable term =
  let go = variableToSrcType rootVariable in
  case term of
    App1 a b ->
        do  a' <- go a
            b' <- go b
            case a' of
              T.App f args ->
                  return $ T.App f (args ++ [b'])
              _ ->
                  return $ T.App a' [b']

    Fun1 a b ->
        T.Lambda <$> go a <*> go b

    Var1 a ->
        go a

    EmptyRecord1 ->
        return $ T.Record [] Nothing

    Record1 tfields extension ->
      do  fields' <- Traverse.traverse (mapM go) tfields
          let fields = concat [ map ((,) name) ts | (name,ts) <- Map.toList fields' ]
          ext' <- dealias <$> go extension
          return $
              case ext' of
                T.Record fs ext -> T.Record (fs ++ fields) ext
                T.Var _ -> T.Record fields (Just ext')
                _ -> error "Used toSrcType on a type that is not well-formed"
      where
        dealias :: T.Canonical -> T.Canonical
        dealias tipe =
            case tipe of
              T.Aliased _ args tipe' ->
                  T.dealias args tipe'
              _ ->
                  tipe


-- ADD NAMES TO TYPES

addNames :: Variable -> IO ()
addNames variable =
  do  usedNames <- getVarNames variable
      let freeNames = makeFreeNames usedNames
      State.runStateT (varAddNames variable) (NameState freeNames 0 0 0)
      return ()


makeFreeNames :: Set.Set String -> [String]
makeFreeNames usedNames =
  let makeName suffix =
          map (:suffix) ['a'..'z']

      allNames =
          concatMap makeName ("" : "'" : "_" : map show [ (0 :: Int) .. ])
  in
      filter (\name -> Set.notMember name usedNames) allNames


data NameState = NameState
    { freeNames :: [String]
    , numberPrimes :: Int
    , comparablePrimes :: Int
    , appendablePrimes :: Int
    }


varAddNames :: Variable -> StateT NameState IO ()
varAddNames var =
  do  desc <- State.liftIO (UF.descriptor var)
      case alias desc of
        Nothing ->
            return ()

        Just (_name, args) ->
            mapM_ (\(v,t) -> (,) v <$> varAddNames t) args

      case structure desc of
        Just term ->
            termAddNames term

        Nothing ->
            case name desc of
              Just _ -> return ()
              Nothing ->
                  do  name' <- createName desc
                      State.liftIO $ UF.modifyDescriptor var $ \desc ->
                          desc { name = Just (Var.local name') }


termAddNames :: Term1 Variable -> StateT NameState IO ()
termAddNames term =
  case term of
    App1 a b ->
        do  varAddNames a
            varAddNames b

    Fun1 a b ->
        do  varAddNames a
            varAddNames b

    Var1 a ->
        varAddNames a

    EmptyRecord1 ->
        return ()

    Record1 fields extension ->
        do  mapM_ varAddNames (concat (Map.elems fields))
            varAddNames extension


createName :: (Monad m) => Descriptor -> StateT NameState m String
createName desc =
  case flex desc of
    Is Number ->
        do  primes <- State.gets numberPrimes
            State.modify (\state -> state { numberPrimes = primes + 1 })
            return ("number" ++ replicate primes '\'')

    Is Comparable ->
        do  primes <- State.gets comparablePrimes
            State.modify (\state -> state { comparablePrimes = primes + 1 })
            return ("comparable" ++ replicate primes '\'')

    Is Appendable ->
        do  primes <- State.gets appendablePrimes
            State.modify (\state -> state { appendablePrimes = primes + 1 })
            return ("appendable" ++ replicate primes '\'')

    _ ->
        do  names <- State.gets freeNames
            State.modify (\state -> state { freeNames = tail names })
            return (head names)


-- GET ALL VARIABLE NAMES

getVarNames :: Variable -> IO (Set.Set String)
getVarNames var =
  do  desc <- UF.descriptor var

      let baseSet =
            case name desc of
              Just var -> Set.singleton (Var.toString var)
              Nothing -> Set.empty

      structureSet <-
          case structure desc of
            Nothing -> return Set.empty
            Just term -> getVarNamesTerm term

      aliasSet <-
          case alias desc of
            Nothing ->
                return Set.empty

            Just (_name, args) ->
                do  let set = Set.fromList (map fst args)
                    sets <- mapM (getVarNames . snd) args
                    return (Set.unions (set : sets))

      return (Set.unions [ baseSet, structureSet, aliasSet ])


getVarNamesTerm :: Term1 Variable -> IO (Set.Set String)
getVarNamesTerm term =
  let go = getVarNames in
  case term of
    App1 a b ->
        Set.union <$> go a <*> go b

    Fun1 a b ->
        Set.union <$> go a <*> go b

    Var1 a ->
        go a

    EmptyRecord1 ->
        return Set.empty

    Record1 fields extension ->
        do  fieldVars <- Set.unions <$> mapM go (concat (Map.elems fields))
            Set.union fieldVars <$> go extension


-- COLLECT APPLICATIONS

data AppStructure
    = List Variable
    | Tuple [Variable]
    | Other


collectApps :: Variable -> IO AppStructure
collectApps var =
    go [] var
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
