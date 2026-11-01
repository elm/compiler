{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, OverloadedStrings #-}
module Parse.Declaration
  ( declaration
  )
  where


import qualified AST.Source as Src
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Parse.Expression as Expr
import qualified Parse.Pattern as Pattern
import qualified Parse.Keyword as Keyword
import qualified Parse.Space as Space
import qualified Parse.Type as Type
import qualified Parse.Variable as Var
import Parse.Primitives hiding (State)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- DECLARATION


declaration :: Space.Parser E.Decl Src.Decl
declaration =
  do  maybeDocs <- chompDocComment
      start <- getPosition
      oneOf E.DeclStart
        [ typeDecl maybeDocs start
        , portDecl maybeDocs start
        , valueDecl maybeDocs start
        ]



-- DOC COMMENT


chompDocComment :: Parser E.Decl (Maybe Src.Comment)
chompDocComment =
  oneOfWithFallback
    [
      do  docComment <- Space.docComment E.DeclStart E.DeclSpace
          Space.chomp E.DeclSpace
          Space.checkFreshLine E.DeclFreshLineAfterDocComment
          return (Just docComment)
    ]
    Nothing



-- DEFINITION and ANNOTATION


{-# INLINE valueDecl #-}
valueDecl :: Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Src.Decl
valueDecl maybeDocs start =
  do  name <- Var.lower N.fromAddr E.DeclStart
      end <- getPosition
      specialize (E.DeclDef name) $
        do  Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
            oneOf E.DeclDefEquals
              [
                do  word1 0x3A#Word8 {-:-} E.DeclDefEquals
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentType
                    (tipe, _) <- specialize E.DeclDefType Type.expression
                    Space.checkFreshLine E.DeclDefNameRepeat
                    defName <- chompMatchingName name
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                    chompDefArgsAndBody maybeDocs start defName (Just tipe) []
              ,
                chompDefArgsAndBody maybeDocs start (A.at start end name) Nothing []
              ]


chompDefArgsAndBody :: Maybe Src.Comment -> A.Position -> A.Located N.Name -> Maybe Src.Type -> [Src.Pattern] -> Space.Parser E.DeclDef Src.Decl
chompDefArgsAndBody maybeDocs start name tipe revArgs =
  oneOf E.DeclDefEquals
    [ do  arg <- specialize E.DeclDefArg Pattern.term
          Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
          chompDefArgsAndBody maybeDocs start name tipe (arg : revArgs)
    , do  word1 0x3D#Word8 {-=-} E.DeclDefEquals
          Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody
          (body, end) <- specialize E.DeclDefBody Expr.expression
          let value = Src.Value name (reverse revArgs) body tipe
          let avalue = A.at start end value
          return (Src.Value_ maybeDocs avalue, end)
    ]


chompMatchingName :: N.Name -> Parser E.DeclDef (A.Located N.Name)
chompMatchingName expectedName =
  let
    (P.Parser k) = Var.lower N.fromAddr E.DeclDefNameRepeat
  in
  P.Parser $ \fpc state@(P.State _ _ _ start) cok eok cerr eerr ->
    let
      cokL name newState@(P.State _ _ _ end) =
        if expectedName == name
        then cok (A.At (A.Region start end) name) newState
        else cerr start (E.DeclDefNameMatch name)

      eokL name newState@(P.State _ _ _ end) =
        if expectedName == name
        then eok (A.At (A.Region start end) name) newState
        else eerr start (E.DeclDefNameMatch name)
    in
    k fpc state cokL eokL cerr eerr



-- TYPE DECLARATIONS


{-# INLINE typeDecl #-}
typeDecl :: Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Src.Decl
typeDecl maybeDocs start =
  inContext E.DeclType (Keyword.type_ E.DeclStart) $
    do  Space.chompAndCheckIndent E.DT_Space E.DT_IndentName
        oneOf E.DT_Name
          [
            inContext E.DT_Alias (Keyword.alias_ E.DT_Name) $
              do  Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                  (name, args) <- chompAliasNameToEquals
                  (tipe, end) <- specialize E.AliasBody Type.expression
                  let alias = A.at start end (Src.Alias name args tipe)
                  return (Src.Alias_ maybeDocs alias, end)
          ,
            specialize E.DT_Union $
              do  (name, args) <- chompCustomNameToEquals
                  (variant, firstEnd) <- Type.variant
                  (variants, end) <- chompVariants [] firstEnd
                  let union = A.at start end (Src.Union name args variant variants)
                  return (Src.Union_ maybeDocs union, end)
          ]



-- TYPE ALIASES


chompAliasNameToEquals :: Parser E.TypeAlias (A.Located T.Name, [A.Located T.Var])
chompAliasNameToEquals =
  do  name <- addLocation (Var.upper T.nameFromAddr E.AliasName)
      Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
      chompAliasNameToEqualsHelp name []


chompAliasNameToEqualsHelp :: A.Located T.Name -> [A.Located T.Var] -> Parser E.TypeAlias (A.Located T.Name, [A.Located T.Var])
chompAliasNameToEqualsHelp name args =
  oneOf E.AliasEquals
    [ do  arg <- addLocation (Var.lower T.varFromAddr E.AliasEquals)
          Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
          chompAliasNameToEqualsHelp name (arg:args)
    , do  word1 0x3D#Word8 {-=-} E.AliasEquals
          Space.chompAndCheckIndent E.AliasSpace E.AliasIndentBody
          return ( name, reverse args )
    ]



-- CUSTOM TYPES


chompCustomNameToEquals :: Parser E.CustomType (A.Located T.Name, [A.Located T.Var])
chompCustomNameToEquals =
  do  name <- addLocation (Var.upper T.nameFromAddr E.CT_Name)
      Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
      chompCustomNameToEqualsHelp name []


chompCustomNameToEqualsHelp :: A.Located T.Name -> [A.Located T.Var] -> Parser E.CustomType (A.Located T.Name, [A.Located T.Var])
chompCustomNameToEqualsHelp name args =
  oneOf E.CT_Equals
    [ do  arg <- addLocation (Var.lower T.varFromAddr E.CT_Equals)
          Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
          chompCustomNameToEqualsHelp name (arg:args)
    , do  word1 0x3D#Word8 {-=-} E.CT_Equals
          Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterEquals
          return ( name, reverse args )
    ]


chompVariants :: [Src.Variant] -> A.Position -> Space.Parser E.CustomType [Src.Variant]
chompVariants variants end =
  oneOfWithFallback
    [ do  Space.checkIndent end E.CT_IndentBar
          word1 0x7C#Word8 {-|-} E.CT_Bar
          Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterBar
          (variant, newEnd) <- Type.variant
          chompVariants (variant:variants) newEnd
    ]
    (reverse variants, end)



-- PORT


{-# INLINE portDecl #-}
portDecl :: Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Src.Decl
portDecl maybeDocs start =
  inContext E.Port (Keyword.port_ E.DeclStart) $
    do  Space.chompAndCheckIndent E.PortSpace E.PortIndentName
        name <- addLocation (Var.lower N.fromAddr E.PortName)
        Space.chompAndCheckIndent E.PortSpace E.PortIndentColon
        word1 0x3A#Word8 {-:-} E.PortColon
        Space.chompAndCheckIndent E.PortSpace E.PortIndentType
        (tipe, end) <- specialize E.PortType Type.expression
        let port_ = A.at start end (Src.Port name tipe)
        return (Src.Port_ maybeDocs port_, end)


