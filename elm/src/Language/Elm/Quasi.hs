{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | This module contains Shakespearean (see "Text.Shakespeare") templates for Elm.
--   It introduces type-safe compile-time variable and URL interpolation. A typeclass
--   @'ToElm'@ is provided for interpolated variables.
--
--   Further reading on Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
--   Further reading on Elm: <http://elm-lang.org> 
module Language.Elm.Quasi
    ( -- * Functions
      -- ** Template-Reading Functions
      -- |These QuasiQuoters return functions of the type @(t -> 'Elm')@
      --  where @t@ is the URL rendering function if type-safe URLs are used.
      --
      --  A usage example for both type-safe (Yesod) and standard path segment (Happstack)
      --  URLs is provided in the Examples folder in the Git repository.
      elm
    , elmFile
    , elmFileReload

      -- * Datatypes
    , Elm (..)

      -- * Typeclass for interpolated variables
    , ToElm (..)

      -- ** Rendering Functions
    , renderElm

    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare

-- | Render Elm to lazy Text.
renderElm :: Elm -> TL.Text
renderElm (Elm b) = toLazyText b

-- | Newtype wrapper of 'Builder'.
newtype Elm = Elm { unElm :: Builder }
    deriving Monoid

-- | A typeclass for types that can be interpolated in Elm templates.
class ToElm a where
    toElm :: a -> Builder
instance ToElm [Char]  where toElm = fromLazyText . TL.pack
instance ToElm TS.Text where toElm = fromText
instance ToElm TL.Text where toElm = fromLazyText

elmSettings :: Q ShakespeareSettings
elmSettings = do
  toJExp <- [|toElm|]
  wrapExp <- [|Elm|]
  unWrapExp <- [|unElm|]
  return $ defaultShakespeareSettings { toBuilder = toJExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  }

-- |QuasiQuoter for embedding Elm code inside of Haskell code.
--
--  Usage:
-- @[elm|main = plaintext "Some elm code"|]@
elm :: QuasiQuoter
elm = QuasiQuoter { quoteExp = \s -> do
    rs <- elmSettings
    quoteExp (shakespeare rs) s
    }

-- |A Template Haskell function for embedding Elm code from external
--  .elm files.
--
--  Usage:
-- @$(elmFile "elm_source/index.elm")@
elmFile :: FilePath -> Q Exp
elmFile fp = do
    rs <- elmSettings
    shakespeareFile rs fp

elmFileReload :: FilePath -> Q Exp
elmFileReload fp = do
    rs <- elmSettings
    shakespeareFileReload rs fp
