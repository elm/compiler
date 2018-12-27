{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Expression
  ( expression
  )
  where


import qualified Data.Name as Name
import Data.Word (Word16)

import qualified AST.Source as Src
import qualified Parse.Keyword as Keyword
import qualified Parse.Number as Number
import qualified Parse.Pattern as Pattern
import qualified Parse.Shader as Shader
import qualified Parse.Symbol as Symbol
import qualified Parse.Type as Type
import qualified Parse.Utf8 as Utf8
import qualified Parse.Variable as Var
import Parse.Utils
import Parse.Primitives hiding (Parser, State)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- TERMS


term :: Parser Src.Expr
term =
  do  start <- getPosition
      oneOf E.XXX
        [ variable start >>= accessible start
        , string start
        , number start
        , Shader.shader start
        , list start
        , record start >>= accessible start
        , tuple start >>= accessible start
        , accessor start
        , character start
        ]


string :: A.Position -> Parser Src.Expr
string start =
  do  str <- Utf8.string
      end <- getPosition
      return (A.at start end (Src.Str str))


character :: A.Position -> Parser Src.Expr
character start =
  do  chr <- Utf8.character
      end <- getPosition
      return (A.at start end (Src.Chr chr))


number :: A.Position -> Parser Src.Expr
number start =
  do  nmbr <- Number.number
      end <- getPosition
      return $ A.at start end $
        case nmbr of
          Number.Int int ->
            Src.Int int

          Number.Float float ->
            Src.Float float


accessor :: A.Position -> Parser Src.Expr
accessor start =
  do  word1 0x2E {-.-} E.XXX
      field <- Var.lower
      end <- getPosition
      return (A.at start end (Src.Accessor field))


variable :: A.Position -> Parser Src.Expr
variable start =
  do  var <- Var.foreignAlpha
      end <- getPosition
      return (A.at start end var)


accessible :: A.Position -> Src.Expr -> Parser Src.Expr
accessible start expr =
  oneOf E.XXX
    [ do  word1 0x2E {-.-} E.XXX
          pos <- getPosition
          field <- Var.lower
          end <- getPosition
          let newExpr = A.at start end (Src.Access expr (A.at pos end field))
          accessible start newExpr
    , return expr
    ]



-- LISTS


list :: A.Position -> Parser Src.Expr
list start =
  inContext E.ExprList (word1 0x5B {-[-} E.XXX) $
    do  spaces
        oneOf E.XXX
          [ do  (entry, _, pos) <- expression
                checkSpace pos
                listHelp start [entry]
          , do  word1 0x5D {-]-} E.XXX
                end <- getPosition
                return (A.at start end (Src.List []))
          ]


listHelp :: A.Position -> [Src.Expr] -> Parser Src.Expr
listHelp start entries =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          listHelp start (entry:entries)
    , do  word1 0x5D {-]-} E.XXX
          end <- getPosition
          return (A.at start end (Src.List (reverse entries)))
    ]



-- TUPLES


tuple :: A.Position -> Parser Src.Expr
tuple start =
  inContext E.ExprTuple (word1 0x28 {-(-} E.XXX) $
  do  pos <- getPosition
      spos <- whitespace
      oneOf E.XXX
        [ do  noSpace pos spos
              word2 0x2D 0x29 {--)-} E.XXX
              end <- getPosition
              return $ A.at start end (Src.Op "-")

        , do  checkSpace spos
              (entry, _, spos2) <- expression
              checkSpace spos2
              tupleHelp start entry []

        , do  noSpace pos spos
              op <- Symbol.binop
              word1 0x29 {-)-} E.XXX
              end <- getPosition
              return $ A.at start end (Src.Op op)

        , do  noSpace pos spos
              word1 0x29 {-)-} E.XXX
              end <- getPosition
              return (A.at start end Src.Unit)

        ]


tupleHelp :: A.Position -> Src.Expr -> [Src.Expr] -> Parser Src.Expr
tupleHelp start firstExpr revExprs =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          tupleHelp start firstExpr (entry : revExprs)
    , do  word1 0x29 {-)-} E.XXX
          end <- getPosition
          case reverse revExprs of
            [] ->
              return firstExpr

            secondExpr : others ->
              return (A.at start end (Src.Tuple firstExpr secondExpr others))
    ]



-- RECORDS


record :: A.Position -> Parser Src.Expr
record start =
  inContext E.ExprRecord (word1 0x7B {- { -} E.XXX) $
    do  spaces
        oneOf E.XXX
          [ do  word1 0x7D {-}-} E.XXX
                end <- getPosition
                return (A.at start end (Src.Record []))
          , do  starter <- addLocation Var.lower
                spaces
                oneOf E.XXX
                  [ do  word1 0x7C {-|-} E.XXX
                        spaces
                        firstField <- chompField
                        fields <- chompFields [firstField]
                        end <- getPosition
                        return (A.at start end (Src.Update starter fields))
                  , do  word1 0x3D {-=-} E.XXX
                        spaces
                        (value, _, nextPos) <- expression
                        checkSpace nextPos
                        fields <- chompFields [(starter, value)]
                        end <- getPosition
                        return (A.at start end (Src.Record fields))
                  ]
          ]


type Field = ( A.Located Name.Name, Src.Expr )


chompFields :: [Field] -> Parser [Field]
chompFields fields =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          f <- chompField
          chompFields (f : fields)
    , do  word1 0x7D {-}-} E.XXX
          return (reverse fields)
    ]


chompField :: Parser Field
chompField =
  do  key <- addLocation Var.lower
      spaces
      word1 0x3D {-=-} E.XXX
      spaces
      (value, _, pos) <- expression
      checkSpace pos
      return (key, value)



-- EXPRESSIONS


type ExprParser =
  SParser Src.Expr


expression :: ExprParser
expression =
  do  start <- getPosition
      oneOf E.XXX
        [ let_ start
        , if_ start
        , case_ start
        , function start
        , do  expr <- possiblyNegativeTerm start
              end <- getPosition
              space <- whitespace
              exprHelp start (State [] expr [] end space)
        ]


data State =
  State
    { _ops  :: ![(Src.Expr, A.Located Name.Name)]
    , _expr :: !Src.Expr
    , _args :: ![Src.Expr]
    , _end  :: !A.Position
    , _pos  :: !SPos
    }


exprHelp :: A.Position -> State -> ExprParser
exprHelp start (State ops expr args end pos) =
  oneOf E.XXX
    [ -- argument
      do  checkSpace pos
          arg <- term
          newEnd <- getPosition
          newPos <- whitespace
          exprHelp start (State ops expr (arg:args) newEnd newPos)

    , -- infix operator
      do  checkSpace pos
          opStart <- getPosition
          opName <- Symbol.binop
          opEnd <- getPosition
          let op = A.at opStart opEnd opName
          spos <- whitespace
          checkSpace spos
          newStart <- getPosition
          if "-" == opName && end /= opStart && opEnd == newStart
            then
              -- negative terms
              do  rawTerm <- term
                  newEnd <- getPosition
                  newPos <- whitespace
                  let arg = A.at opStart newEnd (Src.Negate rawTerm)
                  exprHelp start (State ops expr (arg:args) newEnd newPos)
            else
              oneOf E.XXX
                [ -- term
                  do  newExpr <- possiblyNegativeTerm newStart
                      newEnd <- getPosition
                      newPos <- whitespace
                      let newOps = (toCall expr args, op) : ops
                      exprHelp start (State newOps newExpr [] newEnd newPos)

                , -- final term
                  do  (newLast, newEnd, newPos) <-
                        oneOf E.XXX
                          [ let_ newStart
                          , case_ newStart
                          , if_ newStart
                          , function newStart
                          ]
                      let newOps = (toCall expr args, op) : ops
                      let finalExpr = A.at start newEnd (Src.Binops (reverse newOps) newLast)
                      return ( finalExpr, newEnd, newPos )
                ]

    , -- done
      let finalExpr = toCall expr args in
      case ops of
        [] ->
          return ( finalExpr, end, pos )

        _ ->
          return ( A.at start end (Src.Binops (reverse ops) finalExpr), end, pos )
    ]


possiblyNegativeTerm :: A.Position -> Parser Src.Expr
possiblyNegativeTerm start =
  oneOf E.XXX
    [ do  word1 0x2D {---} E.XXX
          expr <- term
          end <- getPosition
          return (A.at start end (Src.Negate expr))
    , term
    ]


toCall :: Src.Expr -> [Src.Expr] -> Src.Expr
toCall func revArgs =
  case revArgs of
    [] ->
      func

    lastArg : _ ->
      A.merge func lastArg (Src.Call func (reverse revArgs))



-- IF EXPRESSION


if_ :: A.Position -> ExprParser
if_ start =
  inContext E.ExprIf Keyword.if_ $
    ifHelp start []


ifHelp :: A.Position -> [(Src.Expr, Src.Expr)] -> ExprParser
ifHelp start branches =
  do  spaces
      (condition, _, condPos) <- expression
      checkSpace condPos
      Keyword.then_
      spaces
      (thenBranch, _, thenPos) <- expression
      checkSpace thenPos
      Keyword.else_
      spaces
      let newBranches = (condition, thenBranch) : branches
      oneOf E.XXX
        [ do  Keyword.if_
              ifHelp start newBranches
        , do  (elseBranch, elseEnd, elseSpace) <- expression
              let ifExpr = A.at start elseEnd (Src.If (reverse newBranches) elseBranch)
              return ( ifExpr, elseEnd, elseSpace )
        ]



-- LAMBDA EXPRESSION


function :: A.Position -> ExprParser
function start =
  inContext E.ExprFunc (word1 0x5C {-\-} E.XXX) $
    do  spaces
        arg <- Pattern.term
        spaces
        revArgs <- gatherArgs [arg]
        spaces
        (body, end, space) <- expression
        let func = A.at start end (Src.Lambda (reverse revArgs) body)
        return ( func, end, space )


gatherArgs :: [Src.Pattern] -> Parser [Src.Pattern]
gatherArgs args =
  oneOf E.XXX
    [ do  arg <- Pattern.term
          spaces
          gatherArgs (arg:args)
    , do  word2 0x2D 0x3E {-->-} E.XXX
          return args
    ]



-- CASE EXPRESSIONS


case_ :: A.Position -> ExprParser
case_ start =
  inContext E.ExprCase Keyword.case_ $
    do  spaces
        (switcher, _, switcherPos) <- expression
        checkSpace switcherPos
        Keyword.of_
        spaces
        oldIndent <- getIndent
        newIndent <- getCol
        setIndent newIndent
        (firstBranch, firstEnd, firstPos) <- branchHelp
        (branches, end, pos) <- caseHelp [firstBranch] firstEnd firstPos
        setIndent oldIndent
        return
          ( A.at start end (Src.Case switcher branches)
          , end
          , pos
          )


branchHelp :: SParser (Src.Pattern, Src.Expr)
branchHelp =
  do  (pattern, patternPos) <- Pattern.expression
      checkSpace patternPos
      word2 0x2D 0x3E {-->-} E.XXX
      spaces
      (branchExpr, end, pos) <- expression
      return ( (pattern, branchExpr), end, pos )


caseHelp :: [(Src.Pattern, Src.Expr)] -> A.Position -> SPos -> SParser [(Src.Pattern, Src.Expr)]
caseHelp branches end pos =
  oneOf E.XXX
    [ do  checkAligned pos
          (branch, newEnd, newPos) <- branchHelp
          caseHelp (branch:branches) newEnd newPos
    , return ( reverse branches, end, pos )
    ]



-- LET EXPRESSION


let_ :: A.Position -> ExprParser
let_ start =
  do  oldIndent <- getIndent
      letIndent <- getCol
      Keyword.let_
      pushContext start E.ExprLet
      setIndent letIndent
      spaces
      defIndent <- getCol
      setIndent defIndent
      (def, end, space) <- letDef
      letHelp start oldIndent [def] end space


letHelp :: A.Position -> Word16 -> [A.Located Src.Def] -> A.Position -> SPos -> ExprParser
letHelp start oldIndent revDefs end pos =
  oneOf E.XXX
    [ do  checkAligned pos
          (def, newEnd, newPos) <- letDef
          letHelp start oldIndent (def:revDefs) newEnd newPos

    , do  setIndent oldIndent
          checkSpace pos
          Keyword.in_
          popContext ()
          spaces
          (body, newEnd, newPos) <- expression
          let letExpr = A.at start end (Src.Let (reverse revDefs) body)
          return ( letExpr, newEnd, newPos )
    ]



-- LET DEFINITIONS


letDef :: SParser (A.Located Src.Def)
letDef =
  oneOf E.XXX
    [ definition
    , destructure
    ]



-- DEFINITION


definition :: SParser (A.Located Src.Def)
definition =
  do  start <- getPosition
      name <- Var.lower
      end <- getPosition
      spaces
      oneOf E.XXX
        [
          do  word1 0x3A {-:-} E.XXX
              pushContext start (E.Annotation name)
              spaces
              (tipe, _, space) <- Type.expression
              popContext ()
              checkAligned space
              defName <- matchingName name
              let (A.At (A.Region defStart _) _) = defName
              pushContext defStart (E.Definition name)
              spaces
              def <- definitionHelp start defName (Just tipe) []
              popContext def
        ,
          do  pushContext start (E.Definition name)
              def <- definitionHelp start (A.at start end name) Nothing []
              popContext def
        ]


definitionHelp :: A.Position -> A.Located Name.Name -> Maybe Src.Type -> [Src.Pattern] -> SParser (A.Located Src.Def)
definitionHelp start name tipe revArgs =
  oneOf E.XXX
    [ do  arg <- Pattern.term
          spaces
          definitionHelp start name tipe (arg : revArgs)
    , do  word1 0x3D {-=-} E.XXX
          spaces
          (body, end, space) <- expression
          let def = A.at start end (Src.Define name (reverse revArgs) body tipe)
          return ( def, end, space )
    ]


matchingName :: Name.Name -> Parser (A.Located Name.Name)
matchingName expectedName =
  let (P.Parser parserL) = Var.lower in
  P.Parser $ \state@(P.State _ _ _ sr sc ctx) cok eok cerr eerr ->
    let
      cokL name newState@(P.State _ _ _ er ec _) =
        if expectedName == name
        then cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
        else cerr sr sc ctx (E.MatchingName expectedName)

      eokL name newState@(P.State _ _ _ er ec _) =
        if expectedName == name
        then eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
        else eerr sr sc ctx (E.MatchingName expectedName)
    in
    parserL state cokL eokL cerr eerr




-- DESTRUCTURE


destructure :: SParser (A.Located Src.Def)
destructure =
  do  start <- getPosition
      pattern <- Pattern.term
      spaces
      word1 0x3D {-=-} E.XXX
      spaces
      (expr, end, space) <- expression
      return ( A.at start end (Src.Destruct pattern expr), end, space )
