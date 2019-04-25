{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Syntax
  ( Error(..)
  --
  , Decl(..)
  , Infix(..)
  , CustomType(..)
  --
  , Expr(..)
  , Record(..)
  , Tuple(..)
  , List(..)
  , Func(..)
  , Case(..)
  , If(..)
  , Let(..)
  , Def(..)
  , Destruct(..)
  --
  , Pattern(..)
  , PRecord(..)
  , PTuple(..)
  , PList(..)
  --
  , Type(..)
  , TRecord(..)
  , TTuple(..)
  --
  , Char(..)
  , String(..)
  , Escape(..)
  , Number(..)
  --
  , Space(..)
  , Operator(..)
  , toReport
  )
  where


import Prelude hiding (Char, String)
import qualified Data.Char as Char
import qualified Data.Name as Name
import Data.Word (Word16)
--import Numeric (showHex)

import Parse.Primitives (Row, Col)
--import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
--import qualified Reporting.Doc as D
import qualified Reporting.Report as Report
import qualified Reporting.Render.Code as Code



-- ALL SYNTAX ERRORS


data Error
  = CommentOnNothing A.Region
  | UnexpectedPort A.Region
  | NoPorts A.Region
  | ParseError Decl



-- DECLARATIONS


data Decl
  = Module Row Col
  | Import Row Col
  | Annotation Name.Name Type Row Col
  | Definition Name.Name Def Row Col
  | TypeAlias Name.Name Row Col
  | CustomType Name.Name CustomType Row Col
  | Port Name.Name Row Col
  | Infix Infix Row Col
  ----
  | EndlessDocComment Row Col
  | DeclSpace Space Row Col
  | DeclDeadend Row Col


data Infix
  = Precedence Row Col
  | Associativity Row Col


data CustomType
  = CT_Variant Row Col
  | CT_Space Space Row Col
  | CT_Type Type Row Col



-- EXPRESSIONS


data Expr
  = Let Let Row Col
  | Case Case Row Col
  | If If Row Col
  | List List Row Col
  | Record Record Row Col
  | Update Expr Row Col
  | Tuple Tuple Row Col
  | Func Func Row Col
  --
  | Dot Row Col
  | Access Row Col
  | OperatorRHS Row Col
  | OperatorRight Row Col
  | OperatorReserved Operator Row Col
  --
  | Start Row Col
  | Char Char Row Col
  | String String Row Col
  | Number Number Row Col
  | Space Space Row Col
  | EndlessShader Row Col
  | ShaderProblem [Char.Char] Row Col
  | IndentOperatorRight Row Col
  | IndentMoreExpr Row Col


data Record
  = RecordOpen Row Col
  | RecordEnd Row Col
  | RecordField Row Col
  | RecordEquals Row Col
  | RecordExpr Expr Row Col
  | RecordSpace Space Row Col
  --
  | RecordIndentOpen Row Col
  | RecordIndentEnd Row Col
  | RecordIndentField Row Col
  | RecordIndentEquals Row Col
  | RecordIndentExpr Row Col


data Tuple
  = TupleOpen Row Col
  | TupleExpr Expr Row Col
  | TupleSpace Space Row Col
  | TupleEnd Row Col
  | TupleOperatorClose Row Col
  | TupleOperatorReserved Operator Row Col
  --
  | TupleIndentExpr Row Col
  | TupleIndentEnd Row Col


data List
  = ListSpace Space Row Col
  | ListOpen Row Col
  | ListExpr Expr Row Col
  | ListEnd Row Col
  --
  | ListIndentOpen Row Col
  | ListIndentEnd Row Col
  | ListIndentExpr Row Col


data Func
  = FuncSpace Space Row Col
  | FuncArg Pattern Row Col
  | FuncBody Expr Row Col
  | FuncArrow Row Col
  --
  | FuncIndentArg Row Col
  | FuncIndentArrow Row Col
  | FuncIndentBody Row Col


data Case
  = CaseSpace Space Row Col
  | CaseOf Row Col
  | CasePattern Pattern Row Col
  | CaseArrow Row Col
  | CaseExpr Expr Row Col
  | CaseBranch Expr Row Col
  --
  | CaseIndentOf Row Col
  | CaseIndentExpr Row Col
  | CaseIndentPattern Row Col
  | CaseIndentArrow Row Col
  | CaseIndentBranch Row Col
  | CasePatternAlignment Word16 Row Col


data If
  = IfSpace Space Row Col
  | IfThen Row Col
  | IfElse Row Col
  | IfElseBranchStart Row Col
  --
  | IfCondition Expr Row Col
  | IfThenBranch Expr Row Col
  | IfElseBranch Expr Row Col
  --
  | IfIndentCondition Row Col
  | IfIndentThen Row Col
  | IfIndentThenBranch Row Col
  | IfIndentElseBranch Row Col
  | IfIndentElse Row Col


data Let
  = LetSpace Space Row Col
  | LetIn Row Col
  | LetDefAlignment Word16 Row Col
  | LetDefName Row Col
  | LetDef Name.Name Def Row Col
  | LetDestruct Destruct Row Col
  | LetBody Expr Row Col
  | LetIndentDef Row Col
  | LetIndentIn Row Col
  | LetIndentBody Row Col


data Def
  = DefSpace Space Row Col
  | DefType Type Row Col
  | DefNameRepeat Name.Name Row Col
  | DefNameMatch Name.Name Name.Name Row Col
  | DefArg Pattern Row Col
  | DefEquals Row Col
  | DefBody Expr Row Col
  | DefIndentEquals Row Col
  | DefIndentType Row Col
  | DefIndentBody Row Col
  | DefAlignment Word16 Row Col


data Destruct
  = DestructSpace Space Row Col
  | DestructPattern Pattern Row Col
  | DestructEquals Row Col
  | DestructBody Expr Row Col
  | DestructIndentEquals Row Col
  | DestructIndentBody Row Col



-- PATTERNS


data Pattern
  = PRecord PRecord Row Col
  | PTuple PTuple Row Col
  | PList PList Row Col
  --
  | PStart Row Col
  | PChar Char Row Col
  | PString String Row Col
  | PNumber Number Row Col
  | PFloat Int Row Col
  | PAlias Row Col
  | PWildcardNotVar Int Row Col
  | PSpace Space Row Col
  --
  | PIndentStart Row Col
  | PIndentAlias Row Col


data PRecord
  = PRecordOpen Row Col
  | PRecordEnd Row Col
  | PRecordField Row Col
  | PRecordSpace Space Row Col
  --
  | PRecordIndentOpen Row Col
  | PRecordIndentEnd Row Col
  | PRecordIndentField Row Col


data PTuple
  = PTupleOpen Row Col
  | PTupleEnd Row Col
  | PTupleExpr Pattern Row Col
  | PTupleSpace Space Row Col
  --
  | PTupleIndentOpen Row Col
  | PTupleIndentEnd Row Col
  | PTupleIndentExpr Row Col


data PList
  = PListOpen Row Col
  | PListEnd Row Col
  | PListExpr Pattern Row Col
  | PListSpace Space Row Col
  --
  | PListIndentOpen Row Col
  | PListIndentEnd Row Col
  | PListIndentExpr Row Col



-- TYPES


data Type
  = TRecord TRecord Row Col
  | TTuple TTuple Row Col
  | TVariant Row Col
  | TArrow Row Col
  --
  | TStart Row Col
  | TSpace Space Row Col
  --
  | TIndentStart Row Col


data TRecord
  = TRecordOpen Row Col
  | TRecordEnd Row Col
  --
  | TRecordField Row Col
  | TRecordColon Row Col
  | TRecordType Type Row Col
  --
  | TRecordSpace Space Row Col
  --
  | TRecordIndentOpen Row Col
  | TRecordIndentField Row Col
  | TRecordIndentColon Row Col
  | TRecordIndentType Row Col
  | TRecordIndentEnd Row Col


data TTuple
  = TTupleOpen Row Col
  | TTupleEnd Row Col
  | TTupleType Type Row Col
  | TTupleSpace Space Row Col
  --
  | TTupleIndentOpen Row Col
  | TTupleIndentType Row Col
  | TTupleIndentEnd Row Col



-- LITERALS


data Char
  = CharEndless
  | CharEscape Escape
  | CharNotString Int


data String
  = StringEndless_Single
  | StringEndless_Multi
  | StringEscape Escape


data Escape
  = EscapeUnknown Word16 Word16
  | BadUnicodeFormat Word16 Word16
  | BadUnicodeCode Word16 Word16
  | BadUnicodeLength Word16 Word16 Int Int


data Number
  = NumberEnd
  | NumberDot Int
  | NumberHexDigit
  | NumberNoLeadingZero



-- MISC


data Space
  = HasTab
  | EndlessMultiComment
  | UnexpectedDocComment


data Operator
  = OpDot
  | OpPipe
  | OpArrow
  | OpEquals
  | OpHasType



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  error "TODO" source err
