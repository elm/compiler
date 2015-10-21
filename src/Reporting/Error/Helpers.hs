{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Helpers
  ( (|>)
  , functionName, hintLink, stack, reflowParagraph
  , commaSep, capitalize, ordinalize
  , nearbyNames, distance, maybeYouWant
  )
  where

import Data.Function (on)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.EditDistance as Dist
import Text.PrettyPrint.ANSI.Leijen (Doc, (<>), fillSep, hardline, text)

import qualified AST.Helpers as Help
import qualified Elm.Compiler.Version as Compiler
import qualified Elm.Package as Pkg



-- PIPES


(|>) :: a -> (a -> b) -> b
(|>) x f =
  f x


infixl 0 |>



-- DOC HELPERS


functionName :: String -> String
functionName opName =
  if Help.isOp opName then
      "(" ++ opName ++ ")"

  else
      "`" ++ opName ++ "`"


hintLink :: String -> String
hintLink fileName =
  "<https://github.com/elm-lang/elm-compiler/blob/"
  ++ Pkg.versionToString Compiler.version
  ++ "/hints/" ++ fileName ++ ".md>"


stack :: [Doc] -> Doc
stack list =
  case list of
    [] ->
        error "function `stack` requires a non-empty list of docs"

    doc : docs ->
        List.foldl' (\a b -> a <> hardline <> hardline <> b) doc docs


reflowParagraph :: String -> Doc
reflowParagraph paragraph =
  fillSep (map text (words paragraph))


commaSep :: [String] -> String
commaSep tokens =
  case tokens of
    [token] ->
      " " ++ token

    [token1,token2] ->
      " " ++ token1 ++ " and " ++ token2

    _ ->
      " " ++ List.intercalate ", " (init tokens) ++ ", and " ++ last tokens


capitalize :: String -> String
capitalize string =
  case string of
    [] -> []
    c : cs ->
      Char.toUpper c : cs


ordinalize :: Int -> String
ordinalize number =
  let
    remainder10 =
      number `mod` 10

    remainder100 =
      number `mod` 100

    ending
      | remainder100 `elem` [11..13] = "th"
      | remainder10 == 1             = "st"
      | remainder10 == 2             = "nd"
      | remainder10 == 3             = "rd"
      | otherwise                    = "th"
  in
    show number ++ ending



-- NEARBY NAMES


nearbyNames :: (a -> String) -> a -> [a] -> [a]
nearbyNames format name names =
  let editDistance =
        if length (format name) < 3 then 1 else 2
  in
      names
        |> map (\x -> (distance (format name) (format x), x))
        |> List.sortBy (compare `on` fst)
        |> filter ( (<= editDistance) . abs . fst )
        |> map snd


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y


maybeYouWant :: [String] -> String
maybeYouWant suggestions =
  case suggestions of
    [] ->
        ""

    _:_ ->
        "Maybe you want one of the following?\n"
        ++ concatMap ("\n    "++) (take 4 suggestions)

