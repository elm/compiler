{-# LANGUAGE OverloadedStrings #-}
module Reporting.Doc
  ( P.Doc
  , (P.<+>), (<>)
  , P.align, P.cat, P.empty, P.fill, P.fillSep, P.hang
  , P.hcat, P.hsep, P.indent, P.sep, P.vcat
  , P.red, P.cyan, P.magenta, P.green, P.blue, P.black, P.yellow
  , P.dullred, P.dullcyan, P.dullyellow
  --
  , fromChars
  , fromName
  , fromVersion
  , fromPackage
  , fromInt
  --
  , toAnsi
  , toString
  , toLine
  --
  , encode
  --
  , stack
  , reflow
  , commaSep
  --
  , toSimpleNote
  , toFancyNote
  , toSimpleHint
  , toFancyHint
  --
  , link
  , fancyLink
  , reflowLink
  , makeLink
  , makeNakedLink
  --
  , args
  , moreArgs
  , ordinal
  , intToOrdinal
  , cycle
  )
  where


import Prelude hiding (cycle)
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Name as Name
import qualified Data.Text as Text
import qualified Prettyprinter as P (SimpleDocStream(..))
import qualified Prettyprinter.Render.Terminal.Internal as Ansi
import qualified System.Info as Info
import System.IO (Handle)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Data.Index as Index
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import Json.Encode ((==>))
import qualified Json.Encode as E
import qualified Json.String as Json



-- FROM


fromChars :: String -> P.Doc
fromChars =
  P.text


fromName :: Name.Name -> P.Doc
fromName name =
  P.text (Name.toChars name)


fromVersion :: V.Version -> P.Doc
fromVersion vsn =
  P.text (V.toChars vsn)


fromPackage :: Pkg.Name -> P.Doc
fromPackage pkg =
  P.text (Pkg.toChars pkg)


fromInt :: Int -> P.Doc
fromInt n =
  P.text (show n)



-- TO STRING


toAnsi :: Handle -> P.Doc -> IO ()
toAnsi handle doc =
  P.displayIO handle (P.renderPretty 1 80 doc)


toString :: P.Doc -> String
toString doc =
  P.displayS (P.renderPretty 1 80 (P.plain doc)) ""


toLine :: P.Doc -> String
toLine doc =
  P.displayS (P.renderPretty 1 (div maxBound 2) (P.plain doc)) ""



-- FORMATTING


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat (List.intersperse "" docs)


reflow :: String -> P.Doc
reflow paragraph =
  P.fillSep (map P.text (words paragraph))


commaSep :: P.Doc -> (P.Doc -> P.Doc) -> [P.Doc] -> [P.Doc]
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



-- NOTES


toSimpleNote :: String -> P.Doc
toSimpleNote message =
  toFancyNote (map P.text (words message))


toFancyNote :: [P.Doc] -> P.Doc
toFancyNote chunks =
  P.fillSep (P.underline "Note" <> ":" : chunks)



-- HINTS


toSimpleHint :: String -> P.Doc
toSimpleHint message =
  toFancyHint (map P.text (words message))


toFancyHint :: [P.Doc] -> P.Doc
toFancyHint chunks =
  P.fillSep (P.underline "Hint" <> ":" : chunks)



-- LINKS


link :: String -> String -> String -> String -> P.Doc
link word before fileName after =
  P.fillSep $
    (P.underline (P.text word) <> ":")
    : map P.text (words before)
    ++ P.text (makeLink fileName)
    : map P.text (words after)


fancyLink :: String -> [P.Doc] -> String -> [P.Doc] -> P.Doc
fancyLink word before fileName after =
  P.fillSep $
    (P.underline (P.text word) <> ":") : before ++ P.text (makeLink fileName) : after


makeLink :: [Char] -> [Char]
makeLink fileName =
  "<https://elm-lang.org/" <> V.toChars V.compiler <> "/" <> fileName <> ">"


makeNakedLink :: [Char] -> [Char]
makeNakedLink fileName =
  "https://elm-lang.org/" <> V.toChars V.compiler <> "/" <> fileName


reflowLink :: [Char] -> [Char] -> [Char] -> P.Doc
reflowLink before fileName after =
  P.fillSep $
    map P.text (words before)
    ++ P.text (makeLink fileName)
    : map P.text (words after)



-- HELPERS


args :: Int -> String
args n =
  show n <> if n == 1 then " argument" else " arguments"


moreArgs :: Int -> String
moreArgs n =
  show n <> " more" <> if n == 1 then " argument" else " arguments"


ordinal :: Index.ZeroBased -> String
ordinal index =
  intToOrdinal (Index.toHuman index)


intToOrdinal :: Int -> String
intToOrdinal number =
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



cycle :: Int -> Name.Name -> [Name.Name] -> P.Doc
cycle indent name names =
  let
    toLn n = cycleLn <> P.dullyellow (fromName n)
  in
  P.indent indent $ P.vcat $
    cycleTop : List.intersperse cycleMid (toLn name : map toLn names) ++ [ cycleEnd ]


cycleTop, cycleLn, cycleMid, cycleEnd :: P.Doc
cycleTop = if isWindows then "+-----+" else "┌─────┐"
cycleLn  = if isWindows then "|    "   else "│    "
cycleMid = if isWindows then "|     |" else "│     ↓"
cycleEnd = if isWindows then "+-<---+" else "└─────┘"


isWindows :: Bool
isWindows =
  Info.os == "mingw32"



-- JSON


encode :: P.Doc -> E.Value
encode doc =
  E.array (toJsonHelp noStyle [] [] (P.renderPretty 1 80 doc))


type Style = Ansi.AnsiStyle


noStyle :: Style
noStyle =
  mempty


data Color
  = Red
  | RED
  | Magenta
  | MAGENTA
  | Yellow
  | YELLOW
  | Green
  | GREEN
  | Cyan
  | CYAN
  | Blue
  | BLUE
  | Black
  | BLACK
  | White
  | WHITE


toJsonHelp :: Style -> [Style] -> [String] -> P.SimpleDoc -> [E.Value]
toJsonHelp style styles revChunks simpleDoc =
  case simpleDoc of
    P.SFail ->
      error $
        "according to the main implementation, @SFail@ can not\
        \ appear uncaught in a rendered @SimpleDoc@"

    P.SEmpty ->
      [ encodeChunks style revChunks ]

    P.SChar char rest ->
      toJsonHelp style styles ([char] : revChunks) rest

    P.SText _ text rest ->
      toJsonHelp style styles (Text.unpack text : revChunks) rest

    P.SLine indent rest ->
      toJsonHelp style styles (replicate indent ' ' : "\n" : revChunks) rest

    -- Annotations inherit the current style and restore it when popped.
    P.SAnnPush annotation rest ->
      encodeChunks style revChunks : toJsonHelp (annotation <> style) (style : styles) [] rest

    P.SAnnPop rest ->
      case styles of
        outer : others ->
          encodeChunks style revChunks : toJsonHelp outer others [] rest

        [] ->
          error "unbalanced annotation in rendered document"


toColor :: Maybe (Ansi.Intensity, Ansi.Color) -> Maybe Color
toColor foreground =
  case foreground of
    Nothing ->
      Nothing

    Just (intensity, color) ->
      let
        pick dull vivid =
          case intensity of
            Ansi.Dull -> dull
            Ansi.Vivid -> vivid
      in
      Just $
        case color of
          Ansi.Red     -> pick Red     RED
          Ansi.Magenta -> pick Magenta MAGENTA
          Ansi.Yellow  -> pick Yellow  YELLOW
          Ansi.Green   -> pick Green   GREEN
          Ansi.Cyan    -> pick Cyan    CYAN
          Ansi.Blue    -> pick Blue    BLUE
          Ansi.White   -> pick White   WHITE
          Ansi.Black   -> pick Black   BLACK


encodeChunks :: Style -> [String] -> E.Value
encodeChunks style revChunks =
  let
    bold = isJust (Ansi.ansiBold style)
    underline = isJust (Ansi.ansiUnderlining style)
    color = toColor (Ansi.ansiForeground style)
    chars = concat (reverse revChunks)
  in
  case color of
    Nothing | not bold && not underline ->
      E.chars chars

    _ ->
      E.object
        [ "bold" ==> E.bool bold
        , "underline" ==> E.bool underline
        , "color" ==> maybe E.null encodeColor color
        , "string" ==> E.chars chars
        ]


encodeColor :: Color -> E.Value
encodeColor color =
  E.string $ Json.fromChars $
    case color of
      Red -> "red"
      RED -> "RED"
      Magenta -> "magenta"
      MAGENTA -> "MAGENTA"
      Yellow -> "yellow"
      YELLOW -> "YELLOW"
      Green -> "green"
      GREEN -> "GREEN"
      Cyan -> "cyan"
      CYAN -> "CYAN"
      Blue -> "blue"
      BLUE -> "BLUE"
      Black -> "black"
      BLACK -> "BLACK"
      White -> "white"
      WHITE -> "WHITE"
