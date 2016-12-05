{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Expression (term, expression, definition) where

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
import qualified Reporting.Region as R



-- TERMS


term :: Parser Src.RawExpr
term =
  do  start <- getPosition
      oneOf
        [ do  literal <- Literal.literal
              end <- getPosition
              return (A.at start end (Src.Literal literal))
        , list start
        , tuple start
        , record start
        , do  var <- qualifiedVar
              end <- getPosition
              return (A.at start end (Src.var var))
        ]



-- LISTS


list :: R.Position -> Parser Src.RawExpr
list start =
  do  leftSquare
      spaces
      oneOf
        [ do  (entry, _, space) <- expression
              checkSpaces space
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
          (entry, _, space) <- expression
          checkSpaces space
          listHelp start (entry:entries)
    , do  rightSquare
          end <- getPosition
          return (A.at start end (Src.List (reverse entries)))
    ]



-- TUPLES


tuple :: R.Position -> Parser Src.RawExpr
tuple start =
  do  leftParen
      oneOf
          [ do  op <- infixOp
                rightParen
                end <- getPosition
                let x = A.at start end (Src.var "x")
                let y = A.at start end (Src.var "y")
                return $ mkLambda start end "x" $ mkLambda start end "y" $
                  A.at start end (Src.Binop [(x, A.at start end op)] y)

          , do  rightParen
                end <- getPosition
                return (A.at start end (Src.tuple []))

          , do  comma
                arity <- chompCommas 2
                rightParen
                end <- getPosition
                let args = map (\n -> Text.pack ("v" ++ show n)) [ 1 .. arity ]
                let ann x = A.at start end x
                let result = ann (Src.tuple (map (ann . Src.var) args))
                return (foldr (mkLambda start end) result args)

          , do  spaces
                (entry, _, space) <- expression
                checkSpaces space
                tupleHelp start [entry]
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
          (entry, _, space) <- expression
          checkSpaces space
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
      spaces
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
                      (value, _, nextSpace) <- expression
                      checkSpaces nextSpace
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
          return fields
    ]


chompField :: Parser Field
chompField =
  do  key <- addLocation lowVar
      spaces
      equals
      spaces
      (value, _, endSpace) <- expression
      checkSpaces endSpace
      return (key, value)



-- EXPRESSIONS


type ExprParser =
  SParser Src.RawExpr


expression :: ExprParser
expression =
  do  start <- getPosition
      oneOf
        [ let_ start
        , if_ start
        , case_ start
        , function start
        , do  starter <- term
              end <- getPosition
              space <- whitespace
              exprHelp start (State [] starter end space)
        ]


data State =
  State
    { _ops   :: ![(Src.RawExpr, A.Located Text)]
    , _last  :: !Src.RawExpr
    , _end   :: !R.Position
    , _space :: !Space
    }


exprHelp :: R.Position -> State -> ExprParser
exprHelp start (State ops lastExpr end space) =
  oneOf
    [ -- infix operator
      do  checkSpaces space
          op <- addLocation infixOp
          opSpace <- whitespace
          oneOf
            [ -- negative terms
              do  guard (space /= None && opSpace == None && A.drop op == "-")
                  rawTerm <- term
                  newEnd <- getPosition
                  newSpace <- whitespace

                  let ann x = A.merge op rawTerm x
                  let negatedTerm = ann (Src.Binop [(ann Src.zero, op)] rawTerm)
                  let newLast = A.merge lastExpr rawTerm (Src.App lastExpr negatedTerm)

                  exprHelp start (State ops newLast newEnd newSpace)

            , -- accessor
              do  guard (space /= None && opSpace == None && A.drop op == ".")
                  field <- lowVar
                  newEnd <- getPosition
                  newSpace <- whitespace

                  let (A.A (R.Region opStart _) _) = op
                  let ann x = A.at opStart newEnd x
                  let body = ann (Src.Access (ann (Src.var "r")) field)
                  let accessor = mkLambda opStart newEnd "r" body
                  let newLast = A.merge lastExpr accessor (Src.App lastExpr accessor)

                  exprHelp start (State ops newLast newEnd newSpace)

            , -- access
              do  guard (space == None && opSpace == None && A.drop op == ".")
                  field <- lowVar
                  newEnd <- getPosition
                  newSpace <- whitespace
                  let (A.A (R.Region lastStart _) _) = lastExpr
                  let newLast = A.at lastStart newEnd (Src.Access lastExpr field)
                  exprHelp start (State ops newLast newEnd newSpace)

            , -- term
              do  checkSpaces opSpace
                  newLast <- term
                  newEnd <- getPosition
                  newSpace <- whitespace
                  let newOps = (lastExpr, op) : ops
                  exprHelp start (State newOps newLast newEnd newSpace)

            , -- final term
              do  checkSpaces opSpace
                  newStart <- getPosition
                  (newLast, newEnd, newSpace) <-
                    oneOf
                      [ let_ newStart
                      , case_ newStart
                      , if_ newStart
                      , function newStart
                      ]
                  let newOps = (lastExpr, op) : ops
                  let finalExpr = A.at start newEnd (Src.Binop (reverse newOps) newLast)
                  return ( finalExpr, newEnd, newSpace )
            ]

    , -- argument
      do  checkSpaces space
          arg <- term
          newEnd <- getPosition
          newSpace <- whitespace
          let newLast = A.merge lastExpr arg (Src.App lastExpr arg)
          exprHelp start (State ops newLast newEnd newSpace)

    , -- done
      case ops of
        [] ->
          return ( lastExpr, end, space )

        _ ->
          let
            finalExpr =
              A.at start end (Src.Binop (reverse ops) lastExpr)
          in
            return ( finalExpr, end, space )
    ]



-- IF EXPRESSION


if_ :: R.Position -> ExprParser
if_ start =
  ifHelp start []


ifHelp :: R.Position -> [(Src.RawExpr, Src.RawExpr)] -> ExprParser
ifHelp start branches =
  do  keyword "if"
      spaces
      (condition, _, condSpace) <- expression
      checkSpaces condSpace
      keyword "then"
      spaces
      (thenBranch, _, thenSpace) <- expression
      checkSpaces thenSpace
      keyword "else"
      spaces
      let newBranches = (condition, thenBranch) : branches
      oneOf
        [ ifHelp start newBranches
        , do  (elseBranch, elseEnd, elseSpace) <- expression
              let ifExpr = A.at start elseEnd (Src.If (reverse newBranches) elseBranch)
              return ( ifExpr, elseEnd, elseSpace )
        ]



-- LAMBDA EXPRESSION


function :: R.Position -> ExprParser
function start =
  do  lambda
      spaces
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
      spaces
      (switcher, _, switcherSpace) <- expression
      checkSpaces switcherSpace
      keyword "of"
      spaces
      oldIndent <- getIndent
      newIndent <- getCol
      setIndent newIndent
      (firstBranch, firstEnd, firstSpace) <- branchHelp
      (branches, end, space) <- caseHelp [firstBranch] firstEnd firstSpace
      setIndent oldIndent
      return
        ( A.at start end (Src.Case switcher branches)
        , end
        , space
        )


branchHelp :: SParser (P.Raw, Src.RawExpr)
branchHelp =
  do  (pattern, patternSpace) <- Pattern.expression
      checkSpaces patternSpace
      rightArrow
      spaces
      (branchExpr, end, space) <- expression
      return ( (pattern, branchExpr), end, space )


caseHelp :: [(P.Raw, Src.RawExpr)] -> R.Position -> Space -> SParser [(P.Raw, Src.RawExpr)]
caseHelp branches end space =
  case space of
    Aligned ->
      do  (branch, newEnd, newSpace) <- branchHelp
          caseHelp (branch:branches) newEnd newSpace

    _ ->
      return ( reverse branches, end, space )



-- LET EXPRESSION


let_ :: R.Position -> ExprParser
let_ start =
  do  oldIndent <- getIndent
      letIndent <- getCol
      keyword "let"
      setIndent letIndent
      spaces
      defIndent <- getCol
      setIndent defIndent
      (def, end, space) <- definition
      letHelp start oldIndent [def] end space


letHelp :: R.Position -> Int -> [Src.RawDef] -> R.Position -> Space -> ExprParser
letHelp start oldIndent defs end space =
  case space of
    Aligned ->
      do  (def, newEnd, newSpace) <- definition
          letHelp start oldIndent (def:defs) newEnd newSpace

    Freshline ->
      failure (error "TODO missing `in`")

    _ ->
      do  keyword "in"
          setIndent oldIndent
          spaces
          (body, newEnd, newSpace) <- expression
          let letExpr = A.at start end (Src.Let defs body)
          return ( letExpr, newEnd, newSpace )



-- DEFINITIONS


definition :: SParser Src.RawDef
definition =
  do  start <- getPosition
      root <- rootPattern start
      spaces
      case A.drop root of
        P.Var name ->
          oneOf
            [ do  hasType
                  spaces
                  (tipe, end, space) <- Type.expression
                  return ( A.at start end (Src.Annotation name tipe), end, space )
            , definitionHelp start root []
            ]

        _ ->
          definitionHelp start root []


rootPattern :: R.Position -> Parser P.Raw
rootPattern start =
  oneOf
    [ do  op <- try (leftParen >> infixOp)
          rightParen
          end <- getPosition
          return (A.at start end (P.Var op))
    , Pattern.term
    ]


definitionHelp :: R.Position -> P.Raw -> [P.Raw] -> SParser Src.RawDef
definitionHelp start root revArgs =
  oneOf
    [ do  arg <- Pattern.term
          spaces
          definitionHelp start root (arg : revArgs)
    , do  equals
          spaces
          (expr, end, space) <- expression
          let body = List.foldl' (\e x -> A.at start end (Src.Lambda x e)) expr revArgs
          let def = A.at start end (Src.Definition root body)
          return ( def, end, space )
    ]
