{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Language.Elm.Quasi (elm, elmFile) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)

import Data.Attoparsec.Text
import Data.Text as T (Text, pack, unpack, strip)

{- | This QuasiQuoter allows for literal embedding of elm source code within
     Haskell files. It is not responsible for the actual compilation Process.
     Usage:

   @
     elmPage = [elm|
      main = image \"someImage.jpg\"
     |]
   @

     It also allows for simple variable interpolation using the ^{...} syntax.
     This is intended for e.g. embedding pre-rendered type-safe URLs.
     All interpolated variables must be a member of the Show typeclass. This
     is not tested with more complicated expressions, so it is recommended to
     restrict usage of interpolation to pre-calculated variables.

     Example:

   > let google = "http://google.com"
   > in [elm|
   > main = text $ link ^{google} (toText "Google")
   > |]


     How the elm code is compiled depends on how you intend to use it.
     Example uses are the included 'generateHtml' function as well as the
     "Language.Elm.Yesod" module.
-}
elm :: QuasiQuoter
elm = QuasiQuoter
  { quoteExp = \s ->
        let chunks = flip map (getQChunks s) $ \c ->
                        case c of
                            S t -> [| t |]
                            E t -> let Right e = parseExp t in appE [| show |] (return e)
        in appE [| concat |] (listE chunks)
  }

{- | elmFile is a quoteFile wrapper around the 'elm' QuasiQuoter and allows for
     external elm source files to be embedded.
     Usage:

   @
     [elmFile|elm-source/page.elm|]
   @

     Please note that no spaces should be added before and after the filename.
-}
elmFile :: QuasiQuoter
elmFile = quoteFile elm

data QChunk =
      S String -- ^ A literal string
    | E String -- ^ An expression (must be of the Show typeclass)
    deriving (Show, Eq)


{- thanks to mikeplus64 for providing an example QuasiQuoter that I could look at while writing this :) -}
getQChunks :: String -> [QChunk]
getQChunks si = let i = T.pack si 
                    Right m = parseOnly parser (strip i) in m
  where
    parser = go []

    go s = do
        txt <- takeTill (== '^')
        evt <- choice [expression, fmap (S . T.unpack) takeText]
        end <- atEnd
        if end
            then return $ filter (not . blank) $ reverse (evt:(S . T.unpack) txt:s)
            else go (evt:(S . T.unpack) txt:s)

    blank (S "") = True
    blank (E "") = True
    blank _      = False

    expression = do
        string "^{"
        expr <- takeTill (== '}')
        char '}'
        return $ (E . T.unpack) expr