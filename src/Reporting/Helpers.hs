{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Helpers
  ( textToDoc
  , nameToDoc
  , args, moreArgs
  , toSimpleNote, toSimpleHint, toFancyHint
  , link, fancyLink, reflowLink, makeLink
  , stack, reflow
  , commaSep, capitalize, ordinalize, drawCycle
  , findPotentialTypos, findTypoPairs, vetTypos
  , nearbyNames, distance
  -- re-exports
  , Doc, (<+>), (<>), black, blue, cat, dullyellow, fillSep, green, hang, hardline
  , hcat, hsep, indent, magenta, parens, sep, text, underline, vcat
  )
  where


import Data.Function (on)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Elm.Name as N
import qualified Text.EditDistance as Dist
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import qualified Elm.Compiler.Version as Compiler
import qualified Elm.Package as Pkg



-- DOC HELPERS


textToDoc :: Text -> Doc
textToDoc msg =
  text (Text.unpack msg)


nameToDoc :: N.Name -> Doc
nameToDoc name =
  text (N.toString name)


args :: Int -> String
args n =
  show n <> if n == 1 then " argument" else " arguments"


moreArgs :: Int -> String
moreArgs n =
  show n <> " more" <> if n == 1 then " argument" else " arguments"



-- HINTS


toSimpleNote :: String -> Doc
toSimpleNote message =
  fillSep ((underline (text "Note") <> ":") : map text (words message))


toSimpleHint :: String -> Doc
toSimpleHint message =
  toFancyHint (map text (words message))


toFancyHint :: [Doc] -> Doc
toFancyHint chunks =
  fillSep (underline (text "Hint") <> ":" : chunks)


link :: Doc -> String -> String -> String -> Doc
link word before fileName after =
  fillSep $
    (underline word <> ":")
    : map text (words before)
    ++ P.text (makeLink fileName)
    : map text (words after)


fancyLink :: Doc -> [Doc] -> String -> [Doc] -> Doc
fancyLink word before fileName after =
  fillSep $
    (underline word <> ":") : before ++ P.text (makeLink fileName) : after


makeLink :: String -> String
makeLink fileName =
  "<https://elm-lang.org/hints/" <> Pkg.versionToString Compiler.version <> "/" <> fileName <> ">"


reflowLink :: String -> String -> String -> Doc
reflowLink before fileName after =
  fillSep $
    map text (words before)
    ++ P.text (makeLink fileName)
    : map text (words after)


stack :: [Doc] -> Doc
stack chunks =
  case chunks of
    [] ->
        error "function `stack` requires a non-empty list of docs"

    doc : docs ->
        List.foldl' (\a b -> a <> hardline <> hardline <> b) doc docs


reflow :: String -> Doc
reflow paragraph =
  fillSep (map text (words paragraph))


commaSep :: Doc -> (Doc -> Doc) -> [Doc] -> [Doc]
commaSep conjunction addStyle names =
  case names of
    [name] ->
      [ addStyle name ]

    [name1,name2] ->
      [ addStyle name1, conjunction, addStyle name2 ]

    _ ->
      map (\name -> addStyle name <> ",") (init names)
      ++
      [ conjunction
      , addStyle (last names)
      ]


capitalize :: Text -> Text
capitalize txt =
  case Text.uncons txt of
    Nothing ->
      txt

    Just (c, rest) ->
      Text.cons (Char.toUpper c) rest


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
    show number <> ending



drawCycle :: [N.Name] -> Doc
drawCycle names =
  let
    topLine       = "┌─────┐"
    nameLine name = "│    " <> dullyellow (nameToDoc name)
    midLine       = "│     ↓"
    bottomLine    = "└─────┘"
  in
    vcat (topLine : List.intersperse midLine (map nameLine names) ++ [ bottomLine ])



-- FIND TYPOS


findPotentialTypos :: [String] -> String -> [String]
findPotentialTypos knownNames badName =
  filter (\name -> distance badName name == 1) knownNames


findTypoPairs :: [String] -> [String] -> [(String, String)]
findTypoPairs leftOnly rightOnly =
  let
    veryNear leftName =
      map ((,) leftName) (findPotentialTypos rightOnly leftName)
  in
    concatMap veryNear leftOnly


vetTypos :: [(String, String)] -> Maybe (Set.Set String, Set.Set String)
vetTypos potentialTypos =
  let
    tallyNames (ln, rn) (lc, rc) =
      ( Map.insertWith (+) ln 1 lc
      , Map.insertWith (+) rn 1 rc
      )

    (leftCounts, rightCounts) =
      foldr tallyNames (Map.empty, Map.empty) potentialTypos

    acceptable :: Map.Map String Int -> Bool
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


nearbyNames :: (a -> String) -> a -> [a] -> [a]
nearbyNames toString value possibleValues =
  let
    name =
      toString value

    editDistance =
      if length name < 3 then 1 else 2

    getDistance pValue =
      let
        dist = abs (distance name (toString pValue))
      in
      if dist <= editDistance then Just (dist, pValue) else Nothing
  in
  map snd $
    List.sortBy (compare `on` fst) $
      Maybe.mapMaybe getDistance possibleValues


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y
