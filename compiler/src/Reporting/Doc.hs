{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Doc
  ( P.Doc
  , (P.<+>), (<>)
  , P.align, P.cat, P.empty, P.fill, P.fillSep, P.hang
  , P.hcat, P.hsep, P.indent, P.sep, P.vcat
  , P.red, P.cyan, P.magenta, P.green, P.blue, P.black
  , P.dullred, P.dullcyan, P.dullyellow

  , fromString, fromText, fromName, fromInt
  , toAnsi, toString, toLine
  , encode

  , stack, reflow, commaSep
  , toSimpleNote, toSimpleHint, toFancyHint
  , link, fancyLink, reflowLink, makeLink, makeNakedLink
  , args, moreArgs
  , ordinal, intToOrdinal
  , cycle
  )
  where


import Prelude hiding (cycle)
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Elm.Name as N
import qualified System.Console.ANSI.Types as Ansi
import System.IO (Handle)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Data.Index as Index
import qualified Elm.Compiler.Version as Compiler
import qualified Elm.Package as Pkg
import qualified Json.Encode as E



-- FROM


fromString :: String -> P.Doc
fromString =
  P.text


fromText :: Text.Text -> P.Doc
fromText txt =
  P.text (Text.unpack txt)


fromName :: N.Name -> P.Doc
fromName name =
  P.text (N.toString name)


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



-- HINTS


toSimpleNote :: String -> P.Doc
toSimpleNote message =
  P.fillSep ((P.underline "Note" <> ":") : map P.text (words message))


toSimpleHint :: String -> P.Doc
toSimpleHint message =
  toFancyHint (map P.text (words message))


toFancyHint :: [P.Doc] -> P.Doc
toFancyHint chunks =
  P.fillSep (P.underline "Hint" <> ":" : chunks)



-- LINKS


link :: P.Doc -> String -> String -> String -> P.Doc
link word before fileName after =
  P.fillSep $
    (P.underline word <> ":")
    : map P.text (words before)
    ++ P.text (makeLink fileName)
    : map P.text (words after)


fancyLink :: P.Doc -> [P.Doc] -> String -> [P.Doc] -> P.Doc
fancyLink word before fileName after =
  P.fillSep $
    (P.underline word <> ":") : before ++ P.text (makeLink fileName) : after


makeLink :: String -> String
makeLink fileName =
  "<https://elm-lang.org/" <> Pkg.versionToString Compiler.version <> "/" <> fileName <> ">"


makeNakedLink :: String -> String
makeNakedLink fileName =
  "https://elm-lang.org/" <> Pkg.versionToString Compiler.version <> "/" <> fileName


reflowLink :: String -> String -> String -> P.Doc
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



cycle :: Int -> [N.Name] -> P.Doc
cycle indent names =
  let
    topLine       = "┌─────┐"
    nameLine name = "│    " <> P.dullyellow (fromName name)
    midLine       = "│     ↓"
    bottomLine    = "└─────┘"
  in
  P.indent indent $ P.vcat $
    topLine : List.intersperse midLine (map nameLine names) ++ [ bottomLine ]



-- JSON


encode :: P.Doc -> E.Value
encode doc =
  E.array (toJsonHelp noStyle [] (P.renderPretty 1 80 doc))


data Style =
  Style
    { _bold :: Bool
    , _underline :: Bool
    , _color :: Maybe Color
    }


noStyle :: Style
noStyle =
  Style False False Nothing


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


toJsonHelp :: Style -> [TB.Builder] -> P.SimpleDoc -> [E.Value]
toJsonHelp style revChunks simpleDoc =
  case simpleDoc of
    P.SFail ->
      error $
        "according to the main implementation, @SFail@ can not\
        \ appear uncaught in a rendered @SimpleDoc@"

    P.SEmpty ->
      [ encodeChunks style revChunks ]

    P.SChar char rest ->
      toJsonHelp style (TB.singleton char : revChunks) rest

    P.SText _ string rest ->
      toJsonHelp style (TB.fromString string : revChunks) rest

    P.SLine indent rest ->
      toJsonHelp style (spaces indent : "\n" : revChunks) rest

    P.SSGR sgrs rest ->
      encodeChunks style revChunks : toJsonHelp (sgrToStyle sgrs style) [] rest


spaces :: Int -> TB.Builder
spaces n =
  TB.fromText (Text.replicate n " ")


sgrToStyle :: [Ansi.SGR] -> Style -> Style
sgrToStyle sgrs style@(Style bold underline color) =
  case sgrs of
    [] ->
      style

    sgr : rest ->
      sgrToStyle rest $
        case sgr of
          Ansi.Reset                         -> noStyle
          Ansi.SetConsoleIntensity i         -> Style (isBold i) underline color
          Ansi.SetItalicized _               -> style
          Ansi.SetUnderlining u              -> Style bold (isUnderline u) color
          Ansi.SetBlinkSpeed _               -> style
          Ansi.SetVisible _                  -> style
          Ansi.SetSwapForegroundBackground _ -> style
          Ansi.SetColor l i c                -> Style bold underline (toColor l i c)
          Ansi.SetRGBColor _ _               -> style


isBold :: Ansi.ConsoleIntensity -> Bool
isBold intensity =
  case intensity of
    Ansi.BoldIntensity -> True
    Ansi.FaintIntensity -> False
    Ansi.NormalIntensity -> False


isUnderline :: Ansi.Underlining -> Bool
isUnderline underlining =
  case underlining of
    Ansi.SingleUnderline -> True
    Ansi.DoubleUnderline -> False
    Ansi.NoUnderline -> False


toColor :: Ansi.ConsoleLayer -> Ansi.ColorIntensity -> Ansi.Color -> Maybe Color
toColor layer intensity color =
  case layer of
    Ansi.Background ->
      Nothing

    Ansi.Foreground ->
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


encodeChunks :: Style -> [TB.Builder] -> E.Value
encodeChunks (Style bold underline color) revChunks =
  let
    text =
      case revChunks of
        [] ->
          Text.empty

        c:cs ->
          TL.toStrict $ TB.toLazyText $
            List.foldl' (\builder chunk -> chunk <> builder) c cs
  in
  case color of
    Nothing | not bold && not underline ->
      E.text text

    _ ->
      E.object
        [ ("bold", E.bool bold)
        , ("underline", E.bool underline)
        , ("color", maybe E.null encodeColor color)
        , ("string", E.text text)
        ]


encodeColor :: Color -> E.Value
encodeColor color =
  E.text $
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
