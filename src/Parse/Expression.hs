{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Expression (term, expression, topLevelDef) where

import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)

import Parse.Helpers
import qualified Parse.Literal as Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.Type as Type

import qualified AST.Expression.Source as Src
import qualified AST.Pattern as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- TERMS


term :: Parser Src.RawExpr
term =
  hint E.Expr $
  do  start <- getPosition
      oneOf
        [ variable start >>= accessible start
        , literal start
        , Literal.shader start
        , list start
        , record start >>= accessible start
        , tuple start >>= accessible start
        , accessor start
        ]


literal :: R.Position -> Parser Src.RawExpr
literal start =
  do  value <- Literal.literal
      end <- getPosition
      return (A.at start end (Src.Literal value))


accessor :: R.Position -> Parser Src.RawExpr
accessor start =
  do  dot
      field <- lowVar
      end <- getPosition
      let ann x = A.at start end x
      let body = ann (Src.Access (ann (Src.var "r")) field)
      return (mkLambda start end "r" body)


variable :: R.Position -> Parser Src.RawExpr
variable start =
  do  var <- qualifiedVar
      end <- getPosition
      return (A.at start end (Src.var var))


accessible :: R.Position -> Src.RawExpr -> Parser Src.RawExpr
accessible start expr =
  oneOf
    [ do  dot
          field <- lowVar
          end <- getPosition
          let newExpr = A.at start end (Src.Access expr field)
          accessible start newExpr
    , return expr
    ]



-- LISTS


list :: R.Position -> Parser Src.RawExpr
list start =
  do  leftSquare
      inContext start E.ExprList $
        do  spaces
            oneOf
              [ do  (entry, _, pos) <- expression
                    checkSpace pos
                    listHelp start [entry]
              , do  rightSquare
                    end <- getPosition
                    return (A.at start end (Src.List []))
              ]


listHelp :: R.Position -> [Src.RawExpr] -> Parser Src.RawExpr
listHelp start entries =
  oneOf
    [ do  comma
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          listHelp start (entry:entries)
    , do  rightSquare
          end <- getPosition
          return (A.at start end (Src.List (reverse entries)))
    ]



-- TUPLES


tuple :: R.Position -> Parser Src.RawExpr
tuple start =
  do  leftParen
      pos <- getPosition
      spos <- whitespace
      inContext start E.ExprTuple $ oneOf $
        [ do  noSpace pos spos
              try (minus >> rightParen)
              end <- getPosition
              return $ A.at start end (Src.var "-")

        , do  checkSpace spos
              (entry, _, spos2) <- expression
              checkSpace spos2
              tupleHelp start [entry]

        , do  noSpace pos spos
              op <- infixOp
              rightParen
              end <- getPosition
              return $ A.at start end (Src.var op)

        , do  noSpace pos spos
              rightParen
              end <- getPosition
              return (A.at start end (Src.tuple []))

        , do  noSpace pos spos
              comma
              arity <- chompCommas 2
              rightParen
              end <- getPosition
              let args = map (\n -> Text.pack ("v" ++ show n)) [ 1 .. arity ]
              let ann x = A.at start end x
              let result = ann (Src.tuple (map (ann . Src.var) args))
              return (foldr (mkLambda start end) result args)
        ]


mkLambda :: R.Position -> R.Position -> Text -> Src.RawExpr -> Src.RawExpr
mkLambda start end arg body =
  A.at start end (Src.Lambda (A.at start end (P.Var arg)) body)


chompCommas :: Int -> Parser Int
chompCommas n =
  oneOf
    [ comma >> chompCommas (n + 1)
    , return n
    ]


tupleHelp :: R.Position -> [Src.RawExpr] -> Parser Src.RawExpr
tupleHelp start entries =
  oneOf
    [ do  comma
          spaces
          (entry, _, pos) <- expression
          checkSpace pos
          tupleHelp start (entry:entries)
    , do  rightParen
          end <- getPosition
          case entries of
            [entry] ->
              return entry

            _ ->
              return (A.at start end (Src.tuple (reverse entries)))
    ]



-- RECORDS


record :: R.Position -> Parser Src.RawExpr
record start =
  do  leftCurly
      inContext start E.ExprRecord $
        do  spaces
            oneOf
              [ do  rightCurly
                    end <- getPosition
                    return (A.at start end (Src.Record []))
              , do  starter <- addLocation lowVar
                    spaces
                    oneOf
                      [ do  pipe
                            spaces
                            firstField <- chompField
                            fields <- chompFields [firstField]
                            end <- getPosition
                            return (A.at start end (Src.Update (A.map Src.var starter) fields))
                      , do  equals
                            spaces
                            (value, _, nextPos) <- expression
                            checkSpace nextPos
                            fields <- chompFields [(starter, value)]
                            end <- getPosition
                            return (A.at start end (Src.Record fields))
                      ]
              ]


type Field = ( A.Located Text, Src.RawExpr )


chompFields :: [Field] -> Parser [Field]
chompFields fields =
  oneOf
    [ do  comma
          spaces
          f <- chompField
          chompFields (f : fields)
    , do  rightCurly
          return (reverse fields)
    ]


chompField :: Parser Field
chompField =
  do  key <- addLocation lowVar
      spaces
      equals
      spaces
      (value, _, pos) <- expression
      checkSpace pos
      return (key, value)



-- EXPRESSIONS


type ExprParser =
  SParser Src.RawExpr


expression :: ExprParser
expression =
  hint E.Expr $
  do  start <- getPosition
      oneOf
        [ let_ start
        , if_ start
        , case_ start
        , function start
        , do  starter <- possiblyNegativeTerm start
              end <- getPosition
              space <- whitespace
              exprHelp start (State [] starter end space)
        ]


data State =
  State
    { _ops  :: ![(Src.RawExpr, A.Located Text)]
    , _last :: !Src.RawExpr
    , _end  :: !R.Position
    , _pos  :: !SPos
    }


exprHelp :: R.Position -> State -> ExprParser
exprHelp start (State ops lastExpr end pos) =
  oneOf
    [ -- argument
      hint E.Arg $
      do  checkSpace pos
          arg <- term
          newEnd <- getPosition
          newPos <- whitespace
          let newLast = A.merge lastExpr arg (Src.App lastExpr arg)
          exprHelp start (State ops newLast newEnd newPos)

    , -- infix operator
      do  checkSpace pos
          opStart <- getPosition
          opName <- infixOp
          opEnd <- getPosition
          let op = A.at opStart opEnd opName
          spos <- whitespace
          hint (E.AfterOpExpr opName) (checkSpace spos)
          newStart <- getPosition
          hint (E.AfterOpExpr opName) $ oneOf $
            [ -- negative terms
              do  guard ("-" == opName && end /= opStart && opEnd == newStart)
                  rawTerm <- term
                  newEnd <- getPosition
                  newPos <- whitespace

                  let ann x = A.merge op rawTerm x
                  let negatedTerm = ann (Src.Binop [(ann Src.zero, op)] rawTerm)
                  let newLast = A.merge lastExpr rawTerm (Src.App lastExpr negatedTerm)

                  exprHelp start (State ops newLast newEnd newPos)

            , -- term
              do  newLast <- possiblyNegativeTerm newStart
                  newEnd <- getPosition
                  newPos <- whitespace
                  let newOps = (lastExpr, op) : ops
                  exprHelp start (State newOps newLast newEnd newPos)

            , -- final term
              do  (newLast, newEnd, newPos) <-
                    oneOf
                      [ let_ newStart
                      , case_ newStart
                      , if_ newStart
                      , function newStart
                      ]
                  let newOps = (lastExpr, op) : ops
                  let finalExpr = A.at start newEnd (Src.Binop (reverse newOps) newLast)
                  return ( finalExpr, newEnd, newPos )
            ]

    , -- done
      case ops of
        [] ->
          return ( lastExpr, end, pos )

        _ ->
          let
            finalExpr =
              A.at start end (Src.Binop (reverse ops) lastExpr)
          in
            return ( finalExpr, end, pos )
    ]


possiblyNegativeTerm :: R.Position -> Parser Src.RawExpr
possiblyNegativeTerm start =
  oneOf
    [ do  minus
          expr <- term
          end <- getPosition
          let ann x = A.at start end x
          return (ann (Src.Binop [(ann Src.zero, ann "-")] expr))
    , term
    ]



-- IF EXPRESSION


if_ :: R.Position -> ExprParser
if_ start =
  do  keyword "if"
      inContext start E.ExprIf $ ifHelp start []


ifHelp :: R.Position -> [(Src.RawExpr, Src.RawExpr)] -> ExprParser
ifHelp start branches =
  do  spaces
      (condition, _, condPos) <- expression
      checkSpace condPos
      keyword "then"
      spaces
      (thenBranch, _, thenPos) <- expression
      hint E.ElseBranch $ checkSpace thenPos
      hint E.ElseBranch $ keyword "else"
      spaces
      let newBranches = (condition, thenBranch) : branches
      oneOf
        [ do  keyword "if"
              ifHelp start newBranches
        , do  (elseBranch, elseEnd, elseSpace) <- expression
              let ifExpr = A.at start elseEnd (Src.If (reverse newBranches) elseBranch)
              return ( ifExpr, elseEnd, elseSpace )
        ]



-- LAMBDA EXPRESSION


function :: R.Position -> ExprParser
function start =
  do  lambda
      inContext start E.ExprFunc $
        do  spaces
            arg <- Pattern.term
            spaces
            revArgs <- gatherArgs [arg]
            spaces
            (body, end, space) <- expression
            let func = List.foldl' (\e x -> A.at start end (Src.Lambda x e)) body revArgs
            return ( func, end, space )


gatherArgs :: [P.Raw] -> Parser [P.Raw]
gatherArgs args =
  oneOf
    [ do  arg <- Pattern.term
          spaces
          gatherArgs (arg:args)
    , do  rightArrow
          return args
    ]



-- CASE EXPRESSIONS


case_ :: R.Position -> ExprParser
case_ start =
  do  keyword "case"
      inContext start E.ExprCase $
        do  spaces
            (switcher, _, switcherPos) <- expression
            checkSpace switcherPos
            keyword "of"
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


branchHelp :: SParser (P.Raw, Src.RawExpr)
branchHelp =
  do  (pattern, patternPos) <- Pattern.expression
      checkSpace patternPos
      rightArrow
      spaces
      (branchExpr, end, pos) <- expression
      return ( (pattern, branchExpr), end, pos )


caseHelp :: [(P.Raw, Src.RawExpr)] -> R.Position -> SPos -> SParser [(P.Raw, Src.RawExpr)]
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
      keyword "let"
      pushContext start E.ExprLet
      setIndent letIndent
      spaces
      defIndent <- getCol
      setIndent defIndent
      (def, end, space) <- letDef
      letHelp start oldIndent [def] end space


letHelp :: R.Position -> Int -> [Src.RawDef] -> R.Position -> SPos -> ExprParser
letHelp start oldIndent revDefs end pos =
  oneOf
    [ do  checkAligned pos
          (def, newEnd, newPos) <- letDef
          letHelp start oldIndent (def:revDefs) newEnd newPos

    , do  setIndent oldIndent
          checkSpace pos
          keyword "in"
          popContext ()
          spaces
          (body, newEnd, newPos) <- expression
          let letExpr = A.at start end (Src.Let (reverse revDefs) body)
          return ( letExpr, newEnd, newPos )
    ]



-- DEFINITIONS


letDef :: SParser Src.RawDef
letDef =
  definition $ \_ -> Pattern.term


topLevelDef :: SParser Src.RawDef
topLevelDef =
  definition $ \start ->
    oneOf
      [ do  op <- try (leftParen >> infixOp)
            rightParen
            end <- getPosition
            return (A.at start end (P.Var op))
      , Pattern.term
      ]


definition :: (R.Position -> Parser P.Raw) -> SParser Src.RawDef
definition parseRoot =
  do  start <- getPosition
      root <- parseRoot start
      spaces
      case A.drop root of
        P.Var name ->
          oneOf
            [ do  hasType
                  inContext start (E.Annotation name) $
                    do  spaces
                        (tipe, end, space) <- Type.expression
                        return ( A.at start end (Src.Annotation name tipe), end, space )
            , inContext start (E.Definition name) $
                definitionHelp start root []
            ]

        _ ->
          definitionHelp start root []


definitionHelp :: R.Position -> P.Raw -> [P.Raw] -> SParser Src.RawDef
definitionHelp start root revArgs =
  oneOf
    [ do  arg <- hint E.Arg Pattern.term
          spaces
          definitionHelp start root (arg : revArgs)
    , do  equals
          spaces
          (expr, end, space) <- expression
          let body = List.foldl' (\e x -> A.at start end (Src.Lambda x e)) expr revArgs
          let def = A.at start end (Src.Definition root body)
          return ( def, end, space )
    ]
