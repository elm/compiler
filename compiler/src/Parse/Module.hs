{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash #-}
module Parse.Module
  ( fromByteString
  , chompImports
  , chompImport
  )
  where


import qualified Data.ByteString as BS
import qualified Data.List as List
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.Word (Word8(..))

import qualified AST.Source as Src
import qualified AST.Prim.Mode as Mode
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Elm.Compiler.Imports as Imports
import qualified Parse.Declaration as Decl
import qualified Parse.Keyword as Keyword
import qualified Parse.Space as Space
import qualified Parse.Symbol as Symbol
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import Parse.Primitives hiding (State, fromByteString)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- FROM BYTE STRING


fromByteString :: Mode.Mode -> BS.ByteString -> IO (Either E.Error Src.Module)
fromByteString mode source =
  P.fromByteString (chompModule mode) E.ModuleBadEnd source



-- MODULE


chompModule :: Mode.Mode -> Parser E.Error Src.Module
chompModule mode =
  do  header  <- chompHeader
      docs    <- chompDocs header
      imports <- chompImports header (if Mode.isCore mode then [] else Imports.defaults)
      infixes <- if Mode.isKernel mode then chompInfixes else return []
      decls   <- specialize E.Declarations chompDecls
      return $ Src.Module header (toDocs docs decls) imports decls infixes



-- TO DOCS


data Docs
  = YesDocs A.Position Src.Comment A.Position
  | NoDocs A.Region


toDocs :: Docs -> Src.Decls -> Src.Docs
toDocs comment decls =
  case comment of
    YesDocs _ c _ -> getComments decls (Src.YesDocs c)
    NoDocs  r     -> Src.NoDocs r


getComments :: Src.Decls -> ([(N.Name,Src.Comment)] -> [(T.Name,Src.Comment)] -> r) -> r
getComments decls0 done =
  case decls0 of
    Src.NoDecls _     -> done [] []
    Src.RevDecls d ds -> add d ds [] []
  where
    add decl decls vs ts =
      case decl of
        Src.Value_ c (A.At _ (Src.Value n _ _ _)) -> loop decls (addComment c n vs) ts
        Src.Port_  c (A.At _ (Src.Port  n _    )) -> loop decls (addComment c n vs) ts
        Src.Union_ c (A.At _ (Src.Union n _ _ _)) -> loop decls vs (addComment c n ts)
        Src.Alias_ c (A.At _ (Src.Alias n _ _  )) -> loop decls vs (addComment c n ts)

    loop decls vs ts =
      case decls of
        []   -> done vs ts
        d:ds -> add d ds vs ts


addComment :: Maybe Src.Comment -> A.Located name -> [(name, Src.Comment)] -> [(name, Src.Comment)]
addComment maybeComment (A.At _ name) comments =
  case maybeComment of
    Just comment -> (name, comment) : comments
    Nothing      -> comments



--------------------------------------------------------------------------------
-- CHOMPERS --------------------------------------------------------------------
--------------------------------------------------------------------------------



-- CHOMP DECLARATIONS


chompDecls :: Parser E.Decl Src.Decls
chompDecls =
  do  start <- getPosition
      revs <- chompDeclsHelp []
      case revs of
        d:ds -> pure $ Src.RevDecls d ds
        [] ->
          do  end <- getPosition
              pure $ Src.NoDecls $ A.region start end


chompDeclsHelp :: [Src.Decl] -> Parser E.Decl [Src.Decl]
chompDeclsHelp revs =
  oneOf E.DeclStart
    [ do  Space.checkFreshLine E.DeclStart
          (decl, _) <- Decl.declaration
          chompDeclsHelp (decl:revs)
    , pure revs
    ]



-- CHOMP INFIXES


chompInfixes :: Parser E.Error [A.Located Src.Infix]
chompInfixes =
    loop []
  where
    loop infixes =
      oneOfWithFallback
        [ do  Space.checkFreshLine (E.FreshLine E.AfterImport)
              i <- chompInfix
              loop (i:infixes)
        ]
        infixes


-- INVARIANT: always chomps to a freshline
--
chompInfix :: Parser E.Error (A.Located Src.Infix)
chompInfix =
  let
    err = E.Infix
    _err = \_ -> E.Infix
  in
  do  start <- getPosition
      Keyword.infix_ err
      Space.chompAndCheckIndent _err err
      associativity <-
        oneOf err
          [ Keyword.left_  err >> return Op.Left
          , Keyword.right_ err >> return Op.Right
          , Keyword.non_   err >> return Op.Non
          ]
      Space.chompAndCheckIndent _err err
      precedence <- chompPrecedence err
      Space.chompAndCheckIndent _err err
      word1 0x28#Word8 {-(-} err
      op <- addLocation (Symbol.operator err _err)
      word1 0x29#Word8 {-)-} err
      Space.chompAndCheckIndent _err err
      word1 0x3D#Word8 {-=-} err
      Space.chompAndCheckIndent _err err
      name <- Var.lower N.fromAddr err
      end <- getPosition
      Space.chomp _err
      return (A.at start end (Src.Infix op associativity precedence name))


chompPrecedence :: (Cursor -> x) -> Parser x Op.Precedence
chompPrecedence toExpectation =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    if notLtAddr pos end then
      eerr cur toExpectation

    else
      let !word = indexWord8OffAddr# pos 0# in
      if leWord8 0x30#Word8 word && leWord8 word 0x39#Word8
      then
        cok
          (Op.Precedence (W8# (subWord8# word 0x30#Word8 {-0-})))
          (P.State (plusAddr# pos 1#) end indent (slide cur 1#Word64))

      else
        eerr cur toExpectation


{-# INLINE leWord8 #-}
leWord8 :: Word8# -> Word8# -> Bool
leWord8 x y =
  isTrue# (leWord8# x y)



-- CHOMP DOC COMMENT


chompDocs :: Src.Header -> Parser E.Error Docs
chompDocs header =
  case header of
    Src.YesHeader _ _ (A.At (A.Region _ b0) _) ->
      do  (A.Position bN) <- getPosition
          chomp E.AfterHeader (A.Region b0 bN)

    Src.NoHeader blank ->
      chomp E.AfterStart blank
  where
    chomp after blank@(A.Region _ start) =
      do  Space.checkFreshLine (E.FreshLine after)
          oneOfWithFallback
            [
              do  comment <- Space.docComment E.ImportStart E.ModuleSpace
                  end <- getPosition
                  Space.chomp E.ModuleSpace
                  return (YesDocs (A.Position start) comment end)
            ]
            (NoDocs blank)



-- CHOMP HEADER


chompHeader :: Parser E.Error Src.Header
chompHeader =
  do  blank <- P.getRegion $ Space.chomp E.ModuleSpace
      Space.checkFreshLine (E.FreshLine E.AfterStart)
      oneOfWithFallback
        [ specialize E.Module       $                                              header blank Src.Normal
        , specialize E.PortModule   $ Keyword.port_   E.HeaderProblem >> spaces >> header blank Src.Ports
        , specialize E.EffectModule $ Keyword.effect_ E.HeaderProblem >> spaces >> effect blank
        ]
        (Src.NoHeader blank)
  where
    header (A.Region _ start) toStyle =
      do  Keyword.module_ E.HeaderProblem
          (A.Position end) <- getPosition
          spaces
          name <- addLocation (Var.moduleName E.HeaderName)
          spaces
          Keyword.exposing_ E.HeaderProblem
          spaces
          exports <- addLocation (specialize E.HeaderExposing exposing)
          Space.chomp E.HeaderSpace
          return $ Src.YesHeader (toStyle (A.Region start end)) name exports

    spaces =
      Space.chompAndCheckIndent E.HeaderSpace E.HeaderProblem

    effect (A.Region _ start) =
      do  Keyword.module_ E.HeaderProblem
          (A.Position end) <- getPosition
          spaces
          name <- addLocation (Var.moduleName E.HeaderName)
          spaces
          Keyword.where_ E.HeaderProblem
          spaces
          manager <- chompManager
          spaces
          Keyword.exposing_ E.HeaderProblem
          spaces
          exports <- addLocation (specialize E.HeaderExposing exposing)
          Space.chomp E.HeaderSpace
          return $ Src.YesHeader (Src.Effect (A.Region start end) manager) name exports


chompManager :: Parser E.Header Src.Manager
chompManager =
  do  sym 0x7B#Word8 {- { -}
      spaces
      oneOf E.HeaderProblem
        [ do  cmd <- chompCommand
              spaces
              oneOf E.HeaderProblem
                [ do  sym 0x7D#Word8 {-}-}
                      spaces
                      return (Src.Cmd cmd)
                , do  sym 0x2C#Word8 {-,-}
                      spaces
                      sub <- chompSubscription
                      spaces
                      sym 0x7D#Word8 {-}-}
                      spaces
                      return (Src.Fx cmd sub)
                ]
        , do  sub <- chompSubscription
              spaces
              oneOf E.HeaderProblem
                [ do  sym 0x7D#Word8 {-}-}
                      spaces
                      return (Src.Sub sub)
                , do  sym 0x2C#Word8 {-,-}
                      spaces
                      cmd <- chompCommand
                      spaces
                      sym 0x7D#Word8 {-}-}
                      spaces
                      return (Src.Fx cmd sub)
                ]
        ]
  where
    chompCommand =
      do  Keyword.command_ E.HeaderProblem
          spaces
          sym 0x3D#Word8 {-=-}
          spaces
          addLocation (Var.upper T.nameFromAddr E.HeaderProblem)

    chompSubscription =
      do  Keyword.subscription_ E.HeaderProblem
          spaces
          sym 0x3D#Word8 {-=-}
          spaces
          addLocation (Var.upper T.nameFromAddr E.HeaderProblem)

    sym w =
      word1 w E.HeaderProblem

    spaces =
      Space.chompAndCheckIndent E.HeaderSpace E.HeaderProblem



-- CHOMP IMPORTS


chompImports :: Src.Header -> [Src.Import] -> Parser E.Error [Src.Import]
chompImports header defaults =
  case header of
    Src.YesHeader _ _ _ -> loop E.AfterHeader defaults
    Src.NoHeader  _     -> loop E.AfterStart  defaults
  where
    loop after revs =
      oneOfWithFallback
        [ do  Space.checkFreshLine (E.FreshLine after)
              i <- chompImport
              loop E.AfterImport (i:revs)
        ]
        (List.reverse revs)


chompImport :: Parser E.Error Src.Import
chompImport =
  do  Keyword.import_ E.ImportStart
      Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName
      name@(A.At (A.Region _ end) _) <- addLocation (Var.moduleName E.ImportName)
      Space.chomp E.ModuleSpace
      oneOf E.ImportEnd
        [ do  Space.checkFreshLine E.ImportEnd
              return $ Src.Import name Nothing (Src.Explicit [])
        , do  Space.checkIndent (A.Position end) E.ImportEnd
              oneOf E.ImportAs
                [ chompAs name
                , chompExposing name Nothing
                ]
        ]


chompAs :: A.Located Module.Name -> Parser E.Error Src.Import
chompAs name =
  do  Keyword.as_ E.ImportAs
      Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias
      alias <- Var.upper Module.prefixFromAddr E.ImportAlias
      end <- getPosition
      Space.chomp E.ModuleSpace
      oneOf E.ImportEnd
        [ do  Space.checkFreshLine E.ImportEnd
              return $ Src.Import name (Just alias) (Src.Explicit [])
        , do  Space.checkIndent end E.ImportEnd
              chompExposing name (Just alias)
        ]


chompExposing :: A.Located Module.Name -> Maybe Module.Prefix -> Parser E.Error Src.Import
chompExposing name maybeAlias =
  do  Keyword.exposing_ E.ImportExposing
      Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList
      exposed <- specialize E.ImportExposingList exposing
      Space.chomp E.ModuleSpace
      return $ Src.Import name maybeAlias exposed



-- CHOMP EXPOSING


exposing :: Parser E.Exposing Src.Exposing
exposing =
  do  word1 0x28#Word8 {-(-} E.ExposingStart
      Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
      oneOf E.ExposingValue
        [ do  word2 0x2E#Word8 0x2E#Word8 {-..-} E.ExposingValue
              Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
              word1 0x29#Word8 {-)-} E.ExposingEnd
              return Src.Open
        , do  exposed <- chompExposed
              Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
              exposingHelp [exposed]
        ]


exposingHelp :: [Src.Exposed] -> Parser E.Exposing Src.Exposing
exposingHelp revExposed =
  oneOf E.ExposingEnd
    [ do  word1 0x2C#Word8 {-,-} E.ExposingEnd
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
          exposed <- chompExposed
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
          exposingHelp (exposed:revExposed)
    , do  word1 0x29#Word8 {-)-} E.ExposingEnd
          return (Src.Explicit (List.reverse revExposed))
    ]


chompExposed :: Parser E.Exposing Src.Exposed
chompExposed =
  do  start <- getPosition
      oneOf E.ExposingValue
        [
          do  name <- Var.lower N.fromAddr E.ExposingValue
              end <- getPosition
              return $ Src.Lower (A.at start end name)
        ,
          do  word1 0x28#Word8 {-(-} E.ExposingValue
              op <- Symbol.operator E.ExposingOperator E.ExposingOperatorReserved
              word1 0x29#Word8 {-)-} E.ExposingOperatorRightParen
              end <- getPosition
              return $ Src.Operator (A.region start end) op
        ,
          do  name <- Var.upper T.nameFromAddr E.ExposingValue
              end <- getPosition
              Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
              Src.Upper (A.at start end name) <$> privacy
        ]


privacy :: Parser E.Exposing Src.Privacy
privacy =
  oneOfWithFallback
    [ do  word1 0x28#Word8 {-(-} E.ExposingTypePrivacy
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
          start <- getPosition
          word2 0x2E#Word8 0x2E#Word8 {-..-} E.ExposingTypePrivacy
          end <- getPosition
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
          word1 0x29#Word8 {-)-} E.ExposingTypePrivacy
          return $ Src.Public (A.region start end)
    ]
    Src.Private
