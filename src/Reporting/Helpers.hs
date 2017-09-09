{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Helpers
  ( (|>)
  -- re-exports
  , Doc, (<+>), (<>), cat, dullyellow, fillSep, green, hang
  , hsep, indent, parens, sep, text, underline, vcat
  -- custom helpers
  , i2t
  , functionName, args, moreArgs
  , toSimpleHint, toFancyHint, hintLink
  , stack, reflowParagraph
  , commaSep, capitalize, ordinalize, drawCycle
  , findPotentialTypos, findTypoPairs, vetTypos
  , nearbyNames, distance, maybeYouWant, maybeYouWant'
  )
  where

import Data.Function (on)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.EditDistance as Dist
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), text)

import qualified AST.Helpers as Help
import qualified Elm.Compiler.Version as Compiler
import qualified Elm.Package as Pkg



-- PIPES


(|>) :: a -> (a -> b) -> b
(|>) x f =
  f x


infixl 0 |>



-- DOC HELPERS


text :: Text -> Doc
text msg =
  P.text (Text.unpack msg)


i2t :: Int -> Text
i2t n =
  Text.pack (show n)


functionName :: Text -> Text
functionName opName =
  if Help.isOp opName then
      "(" <> opName <> ")"

  else
      "`" <> opName <> "`"


args :: Int -> Text
args n =
  i2t n <> if n == 1 then " argument" else " arguments"


moreArgs :: Int -> Text
moreArgs n =
  i2t n <> " more" <> if n == 1 then " argument" else " arguments"



-- HINTS


toSimpleHint :: Text -> Doc
toSimpleHint txt =
  toFancyHint (map text (Text.words txt))


toFancyHint :: [Doc] -> Doc
toFancyHint chunks =
  fillSep (underline (text "Hint") <> ":" : chunks)


hintLink :: Text -> Text
hintLink fileName =
  "<https://github.com/elm-lang/elm-compiler/blob/"
  <> Text.pack (Pkg.versionToString Compiler.version)
  <> "/hints/" <> fileName <> ".md>"


stack :: [Doc] -> Doc
stack chunks =
  case chunks of
    [] ->
        error "function `stack` requires a non-empty list of docs"

    doc : docs ->
        List.foldl' (\a b -> a <> hardline <> hardline <> b) doc docs


reflowParagraph :: Text -> Doc
reflowParagraph paragraph =
  fillSep (map text (Text.words paragraph))


commaSep :: [Text] -> Text
commaSep tokens =
  case tokens of
    [token] ->
      " " <> token

    [token1,token2] ->
      " " <> token1 <> " and " <> token2

    _ ->
      " " <> Text.intercalate ", " (init tokens) <> ", and " <> last tokens


capitalize :: Text -> Text
capitalize txt =
  case Text.uncons txt of
    Nothing ->
      txt

    Just (c, rest) ->
      Text.cons (Char.toUpper c) rest


ordinalize :: Int -> Text
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
    i2t number <> ending



drawCycle :: [Text] -> Doc
drawCycle names =
  let
    topLine =
        text "┌─────┐"

    nameLine name =
        text "│    " <> dullyellow (text name)

    midLine =
        text "│     ↓"

    bottomLine =
        text "└─────┘"
  in
    vcat (topLine : List.intersperse midLine (map nameLine names) ++ [ bottomLine ])



-- FIND TYPOS


findPotentialTypos :: [Text] -> Text -> [Text]
findPotentialTypos knownNames badName =
  filter ((==1) . distance badName) knownNames


findTypoPairs :: [Text] -> [Text] -> [(Text, Text)]
findTypoPairs leftOnly rightOnly =
  let
    veryNear leftName =
      map ((,) leftName) (findPotentialTypos rightOnly leftName)
  in
    concatMap veryNear leftOnly


vetTypos :: [(Text, Text)] -> Maybe (Set.Set Text, Set.Set Text)
vetTypos potentialTypos =
  let
    tallyNames (ln, rn) (lc, rc) =
      ( Map.insertWith (+) ln 1 lc
      , Map.insertWith (+) rn 1 rc
      )

    (leftCounts, rightCounts) =
      foldr tallyNames (Map.empty, Map.empty) potentialTypos

    acceptable :: Map.Map Text Int -> Bool
    acceptable counts =
      not (Map.null counts)
      &&
      Map.foldr (\n unique -> n < 2 && unique) True counts
  in
    if acceptable leftCounts && acceptable rightCounts then
        Just (Map.keysSet leftCounts, Map.keysSet rightCounts)

    else
        Nothing



-- NEARBY NAMES


nearbyNames :: (a -> Text) -> a -> [a] -> [a]
nearbyNames format name names =
  let editDistance =
        if Text.length (format name) < 3 then 1 else 2
  in
      names
        |> map (\x -> (distance (format name) (format x), x))
        |> List.sortBy (compare `on` fst)
        |> filter ( (<= editDistance) . abs . fst )
        |> map snd


distance :: Text -> Text -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance
    Dist.defaultEditCosts
    (Text.unpack x)
    (Text.unpack y)


maybeYouWant :: Maybe Doc -> [Text] -> Doc
maybeYouWant maybeStarter suggestions =
  maybe P.empty id (maybeYouWant' maybeStarter suggestions)


maybeYouWant' :: Maybe Doc -> [Text] -> Maybe Doc
maybeYouWant' maybeStarter suggestions =
  case suggestions of
    [] ->
      maybeStarter

    _:_ ->
      Just $ stack $
        [ maybe id (<+>) maybeStarter "Maybe you want one of the following?"
        , P.indent 4 $ P.vcat $ map (P.dullyellow . text) (take 4 suggestions)
        ]
