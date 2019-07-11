{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Expression
  ( expression
  )
  where


import qualified Data.Name as Name

import qualified AST.Source as Src
import qualified Parse.Keyword as Keyword
import qualified Parse.Number as Number
import qualified Parse.Pattern as Pattern
import qualified Parse.Shader as Shader
import qualified Parse.Space as Space
import qualified Parse.Symbol as Symbol
import qualified Parse.Type as Type
import qualified Parse.String as String
import qualified Parse.Variable as Var
import Parse.Primitives hiding (State)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- TERMS


term :: Parser E.Expr Src.Expr
term =
  do  start <- getPosition
      oneOf E.Start
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


string :: A.Position -> Parser E.Expr Src.Expr
string start =
  do  str <- String.string E.Start E.String
      addEnd start (Src.Str str)


character :: A.Position -> Parser E.Expr Src.Expr
character start =
  do  chr <- String.character E.Start E.Char
      addEnd start (Src.Chr chr)


number :: A.Position -> Parser E.Expr Src.Expr
number start =
  do  nmbr <- Number.number E.Start E.Number
      addEnd start $
        case nmbr of
          Number.Int int -> Src.Int int
          Number.Float float -> Src.Float float


accessor :: A.Position -> Parser E.Expr Src.Expr
accessor start =
  do  word1 0x2E {-.-} E.Dot
      field <- Var.lower E.Access
      addEnd start (Src.Accessor field)


variable :: A.Position -> Parser E.Expr Src.Expr
variable start =
  do  var <- Var.foreignAlpha E.Start
      addEnd start var


accessible :: A.Position -> Src.Expr -> Parser E.Expr Src.Expr
accessible start expr =
  oneOfWithFallback
    [ do  word1 0x2E {-.-} E.Dot
          pos <- getPosition
          field <- Var.lower E.Access
          end <- getPosition
          accessible start $
            A.at start end (Src.Access expr (A.at pos end field))
    ]
    expr



-- LISTS


list :: A.Position -> Parser E.Expr Src.Expr
list start =
  inContext E.List (word1 0x5B {-[-} E.Start) $
    do  Space.chompAndCheckIndent E.ListSpace E.ListIndentOpen
        oneOf E.ListOpen
          [ do  (entry, end) <- specialize E.ListExpr expression
                Space.checkIndent end E.ListIndentEnd
                chompListEnd start [entry]
          , do  word1 0x5D {-]-} E.ListOpen
                addEnd start (Src.List [])
          ]


chompListEnd :: A.Position -> [Src.Expr] -> Parser E.List Src.Expr
chompListEnd start entries =
  oneOf E.ListEnd
    [ do  word1 0x2C {-,-} E.ListEnd
          Space.chompAndCheckIndent E.ListSpace E.ListIndentExpr
          (entry, end) <- specialize E.ListExpr expression
          Space.checkIndent end E.ListIndentEnd
          chompListEnd start (entry:entries)
    , do  word1 0x5D {-]-} E.ListEnd
          addEnd start (Src.List (reverse entries))
    ]



-- TUPLES


tuple :: A.Position -> Parser E.Expr Src.Expr
tuple start@(A.Position row col) =
  inContext E.Tuple (word1 0x28 {-(-} E.Start) $
    do  before <- getPosition
        Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExpr1
        after <- getPosition
        if before /= after
          then
            do  (entry, end) <- specialize E.TupleExpr expression
                Space.checkIndent end E.TupleIndentEnd
                chompTupleEnd start entry []
          else
            oneOf E.TupleIndentExpr1
              [
                do  op <- Symbol.operator E.TupleIndentExpr1 E.TupleOperatorReserved
                    if op == "-"
                      then
                        oneOf E.TupleOperatorClose
                          [
                            do  word1 0x29 {-)-} E.TupleOperatorClose
                                addEnd start (Src.Op op)
                          ,
                            do  (entry, end) <-
                                  specialize E.TupleExpr $
                                    do  negatedExpr@(A.At (A.Region _ end) _) <- term
                                        Space.chomp E.Space
                                        let exprStart = A.Position row (col + 2)
                                        let expr = A.at exprStart end (Src.Negate negatedExpr)
                                        chompExprEnd exprStart (State [] expr [] end)
                                Space.checkIndent end E.TupleIndentEnd
                                chompTupleEnd start entry []
                          ]
                      else
                        do  word1 0x29 {-)-} E.TupleOperatorClose
                            addEnd start (Src.Op op)
              ,
                do  word1 0x29 {-)-} E.TupleIndentExpr1
                    addEnd start Src.Unit
              ,
                do  (entry, end) <- specialize E.TupleExpr expression
                    Space.checkIndent end E.TupleIndentEnd
                    chompTupleEnd start entry []
              ]


chompTupleEnd :: A.Position -> Src.Expr -> [Src.Expr] -> Parser E.Tuple Src.Expr
chompTupleEnd start firstExpr revExprs =
  oneOf E.TupleEnd
    [ do  word1 0x2C {-,-} E.TupleEnd
          Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExprN
          (entry, end) <- specialize E.TupleExpr expression
          Space.checkIndent end E.TupleIndentEnd
          chompTupleEnd start firstExpr (entry : revExprs)
    , do  word1 0x29 {-)-} E.TupleEnd
          case reverse revExprs of
            [] ->
              return firstExpr

            secondExpr : otherExprs ->
              addEnd start (Src.Tuple firstExpr secondExpr otherExprs)
    ]



-- RECORDS


record :: A.Position -> Parser E.Expr Src.Expr
record start =
  inContext E.Record (word1 0x7B {- { -} E.Start) $
    do  Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen
        oneOf E.RecordOpen
          [ do  word1 0x7D {-}-} E.RecordOpen
                addEnd start (Src.Record [])
          , do  starter <- addLocation (Var.lower E.RecordField)
                Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                oneOf E.RecordEquals
                  [ do  word1 0x7C {-|-} E.RecordEquals
                        Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField
                        firstField <- chompField
                        fields <- chompFields [firstField]
                        addEnd start (Src.Update starter fields)
                  , do  word1 0x3D {-=-} E.RecordEquals
                        Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr
                        (value, end) <- specialize E.RecordExpr expression
                        Space.checkIndent end E.RecordIndentEnd
                        fields <- chompFields [(starter, value)]
                        addEnd start (Src.Record fields)
                  ]
          ]


type Field = ( A.Located Name.Name, Src.Expr )


chompFields :: [Field] -> Parser E.Record [Field]
chompFields fields =
  oneOf E.RecordEnd
    [ do  word1 0x2C {-,-} E.RecordEnd
          Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField
          f <- chompField
          chompFields (f : fields)
    , do  word1 0x7D {-}-} E.RecordEnd
          return (reverse fields)
    ]


chompField :: Parser E.Record Field
chompField =
  do  key <- addLocation (Var.lower E.RecordField)
      Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
      word1 0x3D {-=-} E.RecordEquals
      Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr
      (value, end) <- specialize E.RecordExpr expression
      Space.checkIndent end E.RecordIndentEnd
      return (key, value)



-- EXPRESSIONS


expression :: Space.Parser E.Expr Src.Expr
expression =
  do  start <- getPosition
      oneOf E.Start
        [ let_ start
        , if_ start
        , case_ start
        , function start
        , do  expr <- possiblyNegativeTerm start
              end <- getPosition
              Space.chomp E.Space
              chompExprEnd start (State [] expr [] end)
        ]


data State =
  State
    { _ops  :: ![(Src.Expr, A.Located Name.Name)]
    , _expr :: !Src.Expr
    , _args :: ![Src.Expr]
    , _end  :: !A.Position
    }


chompExprEnd :: A.Position -> State -> Space.Parser E.Expr Src.Expr
chompExprEnd start (State ops expr args end) =
  oneOfWithFallback
    [ -- argument
      do  Space.checkIndent end E.Start
          arg <- term
          newEnd <- getPosition
          Space.chomp E.Space
          chompExprEnd start (State ops expr (arg:args) newEnd)

    , -- operator
      do  Space.checkIndent end E.Start
          op@(A.At (A.Region opStart opEnd) opName) <- addLocation (Symbol.operator E.Start E.OperatorReserved)
          Space.chompAndCheckIndent E.Space (E.IndentOperatorRight opName)
          newStart <- getPosition
          if "-" == opName && end /= opStart && opEnd == newStart
            then
              -- negative terms
              do  negatedExpr <- term
                  newEnd <- getPosition
                  Space.chomp E.Space
                  let arg = A.at opStart newEnd (Src.Negate negatedExpr)
                  chompExprEnd start (State ops expr (arg:args) newEnd)
            else
              let err = E.OperatorRight opName in
              oneOf err
                [ -- term
                  do  newExpr <- possiblyNegativeTerm newStart
                      newEnd <- getPosition
                      Space.chomp E.Space
                      let newOps = (toCall expr args, op) : ops
                      chompExprEnd start (State newOps newExpr [] newEnd)

                , -- final term
                  do  (newLast, newEnd) <-
                        oneOf err
                          [ let_ newStart
                          , case_ newStart
                          , if_ newStart
                          , function newStart
                          ]
                      let newOps = (toCall expr args, op) : ops
                      let finalExpr = Src.Binops (reverse newOps) newLast
                      return ( A.at start newEnd finalExpr, newEnd )
                ]

    ]
    -- done
    (
      case ops of
        [] ->
          ( toCall expr args
          , end
          )

        _ ->
          ( A.at start end (Src.Binops (reverse ops) (toCall expr args))
          , end
          )
    )


possiblyNegativeTerm :: A.Position -> Parser E.Expr Src.Expr
possiblyNegativeTerm start =
  oneOf E.Start
    [ do  word1 0x2D {---} E.Start
          expr <- term
          addEnd start (Src.Negate expr)
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


if_ :: A.Position -> Space.Parser E.Expr Src.Expr
if_ start =
  inContext E.If (Keyword.if_ E.Start) $
    chompIfEnd start []


chompIfEnd :: A.Position -> [(Src.Expr, Src.Expr)] -> Space.Parser E.If Src.Expr
chompIfEnd start branches =
  do  Space.chompAndCheckIndent E.IfSpace E.IfIndentCondition
      (condition, condEnd) <- specialize E.IfCondition expression
      Space.checkIndent condEnd E.IfIndentThen
      Keyword.then_ E.IfThen
      Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch
      (thenBranch, thenEnd) <- specialize E.IfThenBranch expression
      Space.checkIndent thenEnd E.IfIndentElse
      Keyword.else_ E.IfElse
      Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch
      let newBranches = (condition, thenBranch) : branches
      oneOf E.IfElseBranchStart
        [
          do  Keyword.if_ E.IfElseBranchStart
              chompIfEnd start newBranches
        ,
          do  (elseBranch, elseEnd) <- specialize E.IfElseBranch expression
              let ifExpr = Src.If (reverse newBranches) elseBranch
              return ( A.at start elseEnd ifExpr, elseEnd )
        ]



-- LAMBDA EXPRESSION


function :: A.Position -> Space.Parser E.Expr Src.Expr
function start =
  inContext E.Func (word1 0x5C {-\-} E.Start) $
    do  Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArg
        arg <- specialize E.FuncArg Pattern.term
        Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
        revArgs <- chompArgs [arg]
        Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody
        (body, end) <- specialize E.FuncBody expression
        let funcExpr = Src.Lambda (reverse revArgs) body
        return (A.at start end funcExpr, end)


chompArgs :: [Src.Pattern] -> Parser E.Func [Src.Pattern]
chompArgs revArgs =
  oneOf E.FuncArrow
    [ do  arg <- specialize E.FuncArg Pattern.term
          Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
          chompArgs (arg:revArgs)
    , do  word2 0x2D 0x3E {-->-} E.FuncArrow
          return revArgs
    ]



-- CASE EXPRESSIONS


case_ :: A.Position -> Space.Parser E.Expr Src.Expr
case_ start =
  inContext E.Case (Keyword.case_ E.Start) $
    do  Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr
        (expr, exprEnd) <- specialize E.CaseExpr expression
        Space.checkIndent exprEnd E.CaseIndentOf
        Keyword.of_ E.CaseOf
        Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern
        withIndent $
          do  (firstBranch, firstEnd) <- chompBranch
              (branches, end) <- chompCaseEnd [firstBranch] firstEnd
              return
                ( A.at start end (Src.Case expr branches)
                , end
                )


chompBranch :: Space.Parser E.Case (Src.Pattern, Src.Expr)
chompBranch =
  do  (pattern, patternEnd) <- specialize E.CasePattern Pattern.expression
      Space.checkIndent patternEnd E.CaseIndentArrow
      word2 0x2D 0x3E {-->-} E.CaseArrow
      Space.chompAndCheckIndent E.CaseSpace E.CaseIndentBranch
      (branchExpr, end) <- specialize E.CaseBranch expression
      return ( (pattern, branchExpr), end )


chompCaseEnd :: [(Src.Pattern, Src.Expr)] -> A.Position -> Space.Parser E.Case [(Src.Pattern, Src.Expr)]
chompCaseEnd branches end =
  oneOfWithFallback
    [ do  Space.checkAligned E.CasePatternAlignment
          (branch, newEnd) <- chompBranch
          chompCaseEnd (branch:branches) newEnd
    ]
    (reverse branches, end)



-- LET EXPRESSION


let_ :: A.Position -> Space.Parser E.Expr Src.Expr
let_ start =
  inContext E.Let (Keyword.let_ E.Start) $
    do  (defs, defsEnd) <-
          withBacksetIndent 3 $
            do  Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
                withIndent $
                  do  (def, end) <- chompLetDef
                      chompLetDefs [def] end

        Space.checkIndent defsEnd E.LetIndentIn
        Keyword.in_ E.LetIn
        Space.chompAndCheckIndent E.LetSpace E.LetIndentBody
        (body, end) <- specialize E.LetBody expression
        return
          ( A.at start end (Src.Let defs body)
          , end
          )


chompLetDefs :: [A.Located Src.Def] -> A.Position -> Space.Parser E.Let [A.Located Src.Def]
chompLetDefs revDefs end =
  oneOfWithFallback
    [ do  Space.checkAligned E.LetDefAlignment
          (def, newEnd) <- chompLetDef
          chompLetDefs (def:revDefs) newEnd
    ]
    (reverse revDefs, end)



-- LET DEFINITIONS


chompLetDef :: Space.Parser E.Let (A.Located Src.Def)
chompLetDef =
  oneOf E.LetDefName
    [ definition
    , destructure
    ]



-- DEFINITION


definition :: Space.Parser E.Let (A.Located Src.Def)
definition =
  do  aname@(A.At (A.Region start _) name) <- addLocation (Var.lower E.LetDefName)
      specialize (E.LetDef name) $
        do  Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
            oneOf E.DefEquals
              [
                do  word1 0x3A {-:-} E.DefEquals
                    Space.chompAndCheckIndent E.DefSpace E.DefIndentType
                    (tipe, _) <- specialize E.DefType Type.expression
                    Space.checkAligned E.DefAlignment
                    defName <- chompMatchingName name
                    Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                    chompDefArgsAndBody start defName (Just tipe) []
              ,
                chompDefArgsAndBody start aname Nothing []
              ]


chompDefArgsAndBody :: A.Position -> A.Located Name.Name -> Maybe Src.Type -> [Src.Pattern] -> Space.Parser E.Def (A.Located Src.Def)
chompDefArgsAndBody start name tipe revArgs =
  oneOf E.DefEquals
    [ do  arg <- specialize E.DefArg Pattern.term
          Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
          chompDefArgsAndBody start name tipe (arg : revArgs)
    , do  word1 0x3D {-=-} E.DefEquals
          Space.chompAndCheckIndent E.DefSpace E.DefIndentBody
          (body, end) <- specialize E.DefBody expression
          return
            ( A.at start end (Src.Define name (reverse revArgs) body tipe)
            , end
            )
    ]


chompMatchingName :: Name.Name -> Parser E.Def (A.Located Name.Name)
chompMatchingName expectedName =
  let
    (P.Parser parserL) = Var.lower E.DefNameRepeat
  in
  P.Parser $ \state@(P.State _ _ _ _ sr sc) cok eok cerr eerr ->
    let
      cokL name newState@(P.State _ _ _ _ er ec) =
        if expectedName == name
        then cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
        else cerr sr sc (E.DefNameMatch name)

      eokL name newState@(P.State _ _ _ _ er ec) =
        if expectedName == name
        then eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
        else eerr sr sc (E.DefNameMatch name)
    in
    parserL state cokL eokL cerr eerr




-- DESTRUCTURE


destructure :: Space.Parser E.Let (A.Located Src.Def)
destructure =
  specialize E.LetDestruct $
  do  start <- getPosition
      pattern <- specialize E.DestructPattern Pattern.term
      Space.chompAndCheckIndent E.DestructSpace E.DestructIndentEquals
      word1 0x3D {-=-} E.DestructEquals
      Space.chompAndCheckIndent E.DestructSpace E.DestructIndentBody
      (expr, end) <- specialize E.DestructBody expression
      return ( A.at start end (Src.Destruct pattern expr), end )
