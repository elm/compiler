{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | This module contains Shakespearean (see "Text.Shakespeare") templates for Elm.
--   It introduces type-safe compile-time variable and URL interpolation. A typeclass
--   'ToElm' is provided for interpolated variables.
--
--   Further reading on Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
--   Further reading on Elm: <http://elm-lang.org> 
module Language.Elm.Quasi
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'ElmUrl' url@. See the Yesod book for details.
      elm
    , elmFile
    , elmFileReload

      -- * Datatypes
    , ElmUrl
    , Elm (..)

      -- * Typeclass for interpolated variables
    , ToElm (..)

      -- ** Rendering Functions
    , renderElm
    , renderElmUrl

    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare

-- | Render Elm to lazy Text without route interpolation.
renderElm :: Elm -> TL.Text
renderElm (Elm b) = toLazyText b

-- | render with route interpolation. If using this module standalone, apart
-- from type-safe routes, a dummy renderer can be used:
-- 
-- > renderElmUrl (\_ _ -> undefined) elmUrl
--
-- When using Yesod, a renderer is generated for you, which can be accessed
-- within the GHandler monad: 'Yesod.Handler.getUrlRenderParams'.
renderElmUrl :: (url -> [(TS.Text, TS.Text)] -> TS.Text) -> ElmUrl url -> TL.Text
renderElmUrl r s = renderElm $ s r

-- | Newtype wrapper of 'Builder'.
newtype Elm = Elm { unElm :: Builder }
    deriving Monoid

-- | Return type of template-reading functions.
type ElmUrl url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Elm

-- | A typeclass for types that can be interpolated in CoffeeScript templates.
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

elm :: QuasiQuoter
elm = QuasiQuoter { quoteExp = \s -> do
    rs <- elmSettings
    quoteExp (shakespeare rs) s
    }

elmFile :: FilePath -> Q Exp
elmFile fp = do
    rs <- elmSettings
    shakespeareFile rs fp

elmFileReload :: FilePath -> Q Exp
elmFileReload fp = do
    rs <- elmSettings
    shakespeareFileReload rs fp
