{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Expression
  ( expression
  )
  where


import Control.Monad (guard)

import qualified AST.Source as Src
import qualified Elm.Name as N
import Parse.Primitives
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Number as Number
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Utf8 as Utf8
import qualified Parse.Primitives.Variable as Var
import qualified Parse.Primitives.Whitespace as W
import qualified Parse.Pattern as Pattern
import qualified Parse.Shader as Shader
import qualified Parse.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- TERMS


term :: Parser Src.Expr
term =
  hint E.Expr $
  do  start <- getPosition
      oneOf
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


string :: R.Position -> Parser Src.Expr
string start =
  do  str <- Utf8.string
      end <- getPosition
      return (A.at start end (Src.Str str))


character :: R.Position -> Parser Src.Expr
character start =
  do  chr <- Utf8.character
      end <- getPosition
      return (A.at start end (Src.Chr chr))


number :: R.Position -> Parser Src.Expr
number start =
  do  nmbr <- Number.number
      end <- getPosition
      return $ A.at start end $
        case nmbr of
          Number.Int int ->
            Src.Int int

          Number.Float float ->
            Src.Float float


accessor :: R.Position -> Parser Src.Expr
accessor start =
  do  Symbol.dot
      field <- Var.lower
      end <- getPosition
      return (A.at start end (Src.Accessor field))


variable :: R.Position -> Parser Src.Expr
variable start =
  do  var <- Var.foreignAlpha
      end <- getPosition
      return (A.at start end var)


accessible :: R.Position -> Src.Expr -> Parser Src.Expr
accessible start expr =
  oneOf
    [ do  Symbol.dot
          pos <- getPosition
          field <- Var.lower
          end <- getPosition
          let newExpr = A.at start end (Src.Access expr (A.at pos end field))
          accessible start newExpr
    , return expr
    ]



-- LISTS


list :: R.Position -> Parser Src.Expr
list start =
  do  Symbol.leftSquare
      inContext start E.ExprList $
        do  spaces
            oneOf
              [ do  (entry, _, pos) <- expression
                    checkSpace pos
                    listHelp start [entry]
              , do  Symbol.rightSquare
                    end <- getPosition
                    return (A.at start end (Src.List []))
              ]


listHelp :: R.Position -> [Src.Expr] -> Parser Src.Expr
listHelp start entries =
  oneOf
    [ do  Symbol.comma
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          listHelp start (entry:entries)
    , do  Symbol.rightSquare
          end <- getPosition
          return (A.at start end (Src.List (reverse entries)))
    ]



-- TUPLES


tuple :: R.Position -> Parser Src.Expr
tuple start =
  do  Symbol.leftParen
      pos <- getPosition
      spos <- W.whitespace
      inContext start E.ExprTuple $ oneOf $
        [ do  noSpace pos spos
              try (Symbol.minus >> Symbol.rightParen)
              end <- getPosition
              return $ A.at start end (Src.Op "-")

        , do  checkSpace spos
              (entry, _, spos2) <- expression
              checkSpace spos2
              tupleHelp start entry []

        , do  noSpace pos spos
              op <- Symbol.binop
              Symbol.rightParen
              end <- getPosition
              return $ A.at start end (Src.Op op)

        , do  noSpace pos spos
              Symbol.rightParen
              end <- getPosition
              return (A.at start end Src.Unit)

        ]


tupleHelp :: R.Position -> Src.Expr -> [Src.Expr] -> Parser Src.Expr
tupleHelp start firstExpr revExprs =
  oneOf
    [ do  Symbol.comma
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          tupleHelp start firstExpr (entry : revExprs)
    , do  Symbol.rightParen
          end <- getPosition
          case reverse revExprs of
            [] ->
              return firstExpr

            secondExpr : others ->
              return (A.at start end (Src.Tuple firstExpr secondExpr others))
    ]



-- RECORDS


record :: R.Position -> Parser Src.Expr
record start =
  do  Symbol.leftCurly
      inContext start E.ExprRecord $
        do  spaces
            oneOf
              [ do  Symbol.rightCurly
                    end <- getPosition
                    return (A.at start end (Src.Record []))
              , do  starter <- addLocation Var.lower
                    spaces
                    oneOf
                      [ do  Symbol.pipe
                            spaces
                            firstField <- chompField
                            fields <- chompFields [firstField]
                            end <- getPosition
                            return (A.at start end (Src.Update starter fields))
                      , do  Symbol.equals
                            spaces
                            (value, _, nextPos) <- expression
                            checkSpace nextPos
                            fields <- chompFields [(starter, value)]
                            end <- getPosition
                            return (A.at start end (Src.Record fields))
                      ]
              ]


type Field = ( A.Located N.Name, Src.Expr )


chompFields :: [Field] -> Parser [Field]
chompFields fields =
  oneOf
    [ do  Symbol.comma
          spaces
          f <- chompField
          chompFields (f : fields)
    , do  Symbol.rightCurly
          return (reverse fields)
    ]


chompField :: Parser Field
chompField =
  do  key <- addLocation Var.lower
      spaces
      Symbol.equals
      spaces
      (value, _, pos) <- expression
      checkSpace pos
      return (key, value)



-- EXPRESSIONS


type ExprParser =
  SParser Src.Expr


expression :: ExprParser
expression =
  hint E.Expr $
  do  start <- getPosition
      oneOf
        [ let_ start
        , if_ start
        , case_ start
        , function start
        , do  expr <- possiblyNegativeTerm start
              end <- getPosition
              space <- W.whitespace
              exprHelp start (State [] expr [] end space)
        ]


data State =
  State
    { _ops  :: ![(Src.Expr, A.Located N.Name)]
    , _expr :: !Src.Expr
    , _args :: ![Src.Expr]
    , _end  :: !R.Position
    , _pos  :: !SPos
    }


exprHelp :: R.Position -> State -> ExprParser
exprHelp start (State ops expr args end pos) =
  oneOf
    [ -- argument
      hint E.Arg $
      do  checkSpace pos
          arg <- term
          newEnd <- getPosition
          newPos <- W.whitespace
          exprHelp start (State ops expr (arg:args) newEnd newPos)

    , -- infix operator
      do  checkSpace pos
          opStart <- getPosition
          opName <- Symbol.binop
          opEnd <- getPosition
          let op = A.at opStart opEnd opName
          spos <- W.whitespace
          hint (E.AfterOpExpr opName) (checkSpace spos)
          newStart <- getPosition
          hint (E.AfterOpExpr opName) $ oneOf $
            [ -- negative terms
              do  guard ("-" == opName && end /= opStart && opEnd == newStart)
                  rawTerm <- term
                  newEnd <- getPosition
                  newPos <- W.whitespace
                  let arg = A.at opStart newEnd (Src.Negate rawTerm)
                  exprHelp start (State ops expr (arg:args) newEnd newPos)

            , -- term
              do  newExpr <- possiblyNegativeTerm newStart
                  newEnd <- getPosition
                  newPos <- W.whitespace
                  let newOps = (toCall expr args, op) : ops
                  exprHelp start (State newOps newExpr [] newEnd newPos)

            , -- final term
              do  (newLast, newEnd, newPos) <-
                    oneOf
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


possiblyNegativeTerm :: R.Position -> Parser Src.Expr
possiblyNegativeTerm start =
  oneOf
    [ do  Symbol.minus
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


if_ :: R.Position -> ExprParser
if_ start =
  do  Keyword.if_
      inContext start E.ExprIf $ ifHelp start []


ifHelp :: R.Position -> [(Src.Expr, Src.Expr)] -> ExprParser
ifHelp start branches =
  do  spaces
      (condition, _, condPos) <- expression
      checkSpace condPos
      Keyword.then_
      spaces
      (thenBranch, _, thenPos) <- expression
      hint E.ElseBranch $ checkSpace thenPos
      hint E.ElseBranch $ Keyword.else_
      spaces
      let newBranches = (condition, thenBranch) : branches
      oneOf
        [ do  Keyword.if_
              ifHelp start newBranches
        , do  (elseBranch, elseEnd, elseSpace) <- expression
              let ifExpr = A.at start elseEnd (Src.If (reverse newBranches) elseBranch)
              return ( ifExpr, elseEnd, elseSpace )
        ]



-- LAMBDA EXPRESSION


function :: R.Position -> ExprParser
function start =
  do  Symbol.lambda
      inContext start E.ExprFunc $
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
  oneOf
    [ do  arg <- Pattern.term
          spaces
          gatherArgs (arg:args)
    , do  Symbol.rightArrow
          return args
    ]



-- CASE EXPRESSIONS


case_ :: R.Position -> ExprParser
case_ start =
  do  Keyword.case_
      inContext start E.ExprCase $
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
      Symbol.rightArrow
      spaces
      (branchExpr, end, pos) <- expression
      return ( (pattern, branchExpr), end, pos )


caseHelp :: [(Src.Pattern, Src.Expr)] -> R.Position -> SPos -> SParser [(Src.Pattern, Src.Expr)]
caseHelp branches end pos =
  oneOf
    [ do  checkAligned pos
          (branch, newEnd, newPos) <- branchHelp
          caseHelp (branch:branches) newEnd newPos
    , return ( reverse branches, end, pos )
    ]



-- LET EXPRESSION


let_ :: R.Position -> ExprParser
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


letHelp :: R.Position -> Int -> [A.Located Src.Def] -> R.Position -> SPos -> ExprParser
letHelp start oldIndent revDefs end pos =
  oneOf
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
  oneOf
    [ definition
    , destructure
    ]



-- DEFINITION


definition :: SParser (A.Located Src.Def)
definition =
  do  start <- getPosition
      name <- Var.lower
      nameEnd <- getPosition
      spaces
      oneOf
        [ do  Symbol.hasType
              inContext start (E.Annotation name) $
                do  spaces
                    (tipe, end, space) <- Type.expression
                    return ( A.at start end (Src.Annotate name tipe), end, space )
        , inContext start (E.Definition name) $
            definitionHelp start (A.at start nameEnd name) []
        ]


definitionHelp :: R.Position -> A.Located N.Name -> [Src.Pattern] -> SParser (A.Located Src.Def)
definitionHelp start name revArgs =
  oneOf
    [ do  arg <- hint E.Arg Pattern.term
          spaces
          definitionHelp start name (arg : revArgs)
    , do  Symbol.equals
          spaces
          (body, end, space) <- expression
          let def = A.at start end (Src.Define name (reverse revArgs) body)
          return ( def, end, space )
    ]



-- DESTRUCTURE


destructure :: SParser (A.Located Src.Def)
destructure =
  do  start <- getPosition
      pattern <- Pattern.term
      spaces
      Symbol.equals
      spaces
      (expr, end, space) <- expression
      return ( A.at start end (Src.Destruct pattern expr), end, space )
